#include "BuildCommand.h"

#include <antlr4-runtime.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>

#include <filesystem>
#include <fstream>
#include <system_error>

#include "../llvm/Executer.h"
#include "../sema/SemaBuilder.h"
#include "ClearLanguageLexer.h"

#ifdef HAVE_LLD
#include <lld/Common/Driver.h>  // lldMain, DriverDef
#if defined(_WIN32)
namespace lld {
namespace coff {
bool link(llvm::ArrayRef<const char*>, llvm::raw_ostream&, llvm::raw_ostream&,
          bool /*ExitEarly*/, bool /*DisableOutput*/);
}  // namespace coff
}  // namespace lld
#endif
#endif

#ifdef HAVE_LLD
static void link_with_lld(const std::string& obj_path,
                          const std::string& out_path, const bool as_dll) {
#if defined(_WIN32)
  llvm::SmallVector<const char*, 16> argv;
  std::vector<std::string> storage;
  auto add = [&](std::string s) -> const char* {
    storage.push_back(std::move(s));
    return storage.back().c_str();
  };

  argv.push_back("lld-link");
  argv.push_back("/nologo");
  argv.push_back(add("/out:" + out_path));
  if (as_dll) {
    argv.push_back("/dll");
  } else {
    argv.push_back("/subsystem:console");
  }

#ifdef CLEAR_RUNTIME_LIB_DIR
#ifdef CLEAR_RUNTIME_LIB_NAME
  const std::string rtlib =
      std::string(CLEAR_RUNTIME_LIB_DIR) + "/" + CLEAR_RUNTIME_LIB_NAME;
  argv.push_back(add(rtlib));
#endif
#endif

#ifdef CLEAR_AOT_ENTRY_LIB_DIR
#ifdef CLEAR_AOT_ENTRY_LIB_NAME
  const std::string entrylib =
      std::string(CLEAR_AOT_ENTRY_LIB_DIR) + "/" + CLEAR_AOT_ENTRY_LIB_NAME;
  argv.push_back(add(entrylib));
#endif
#endif

  argv.push_back(add(obj_path));

  const lld::DriverDef drivers_arr[] = {
      {lld::Flavor::WinLink, &lld::coff::link}};
  const auto drivers = llvm::ArrayRef<lld::DriverDef>(drivers_arr, 1);

  auto rc = lld::lldMain(argv, llvm::outs(), llvm::errs(), drivers);
  if (rc.retCode != 0) {
    throw std::runtime_error("lld::lldMain (COFF) failed");
  }
#endif
}
#endif  // HAVE_LLD

build_command::build_command(std::string build_script_path)
    : build_script_path_(std::move(build_script_path)) {
  parse_build_script();
}

void build_command::parse_build_script() {
  std::ifstream file(build_script_path_);
  if (!file.is_open()) {
    throw std::runtime_error("build.clr not found");
  }

  antlr4::ANTLRInputStream input(file);
  ClearLanguageLexer lexer(&input);
  antlr4::CommonTokenStream tokens(&lexer);
  ClearLanguageParser parser(&tokens);

  auto* tree = parser.start();

  sema_builder sb;

  sb.collect_signatures(tree);
  sb.construct_target(tree);

  auto mod = sb.take_module();
  extract_build_functions(*mod);
}

void build_command::extract_build_functions(const sema::module& mod) {
  for (const auto& func : mod.functions) {
    const bool is_config =
        func->name == "configure" || func->name == "build::configure" ||
        (func->name.size() > 11 &&
         func->name.substr(func->name.size() - 10) == "::configure");

    if (!is_config) continue;

    for (const auto& stmt : func->body->statements) {
      if (const auto* expr_stmt = dynamic_cast<sema::stmt_expr*>(stmt.get())) {
        if (const auto* call =
                dynamic_cast<sema::call*>(expr_stmt->expr.get())) {
          if (call->callee == "__set_entry") {
            if (const auto* lit =
                    dynamic_cast<sema::literal*>(call->args[0].get())) {
              entry_point_ = as_string(lit->value);
            }
          }
          if (call->callee == "__add_source") {
            if (const auto* lit =
                    dynamic_cast<sema::literal*>(call->args[0].get())) {
              source_root_ = as_string(lit->value);
            }
          }
          if (call->callee == "__set_output") {
            if (const auto* lit =
                    dynamic_cast<sema::literal*>(call->args[0].get())) {
              output_path_ = as_string(lit->value);
            }
          }
          if (call->callee == "__set_target") {
            if (const auto* lit =
                    dynamic_cast<sema::literal*>(call->args[0].get())) {
              const auto k = as_string(lit->value);
              if (k == "exe" || k == "executable") {
                kind_ = build_kind::exe;
              } else if (k == "dll" || k == "shared") {
                kind_ = build_kind::dll;
              } else if (k == "none") {
                kind_ = build_kind::none;
              } else {
                throw std::runtime_error("unknown target kind: " + k);
              }
            }
          }
          if (call->callee == "__set_app_name") {
            if (const auto* lit =
                    dynamic_cast<sema::literal*>(call->args[0].get())) {
              app_name_ = as_string(lit->value);
            }
          }
        }
      }
    }
  }
}

std::vector<std::string> build_command::collect_source_files(
    const std::string& root) {
  std::vector<std::string> files;
  for (auto& p : std::filesystem::recursive_directory_iterator(root)) {
    if (!p.is_regular_file()) continue;
    if (p.path().extension() == ".clr" && p.path().filename() != "build.clr")
      files.push_back(p.path().string());
  }
  return files;
}

static void emit_object_file(llvm::Module& m, const std::string& obj_path) {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  const std::string triple = llvm::sys::getDefaultTargetTriple();
  m.setTargetTriple(triple);

  std::string err;
  const llvm::Target* target = llvm::TargetRegistry::lookupTarget(triple, err);
  if (!target) {
    throw std::runtime_error("lockupTarget failed: " + err);
  }

  llvm::TargetOptions opt;
  std::unique_ptr<llvm::TargetMachine> tm(
      target->createTargetMachine(triple, "generic", "", opt, std::nullopt));

  if (!tm) throw std::runtime_error("createTargetMachine failed");

  m.setDataLayout(tm->createDataLayout());

  std::error_code ec;
  llvm::raw_fd_ostream dest(obj_path, ec, llvm::sys::fs::OF_None);
  if (ec) throw std::runtime_error("open object file failed: " + ec.message());

  llvm::legacy::PassManager pass;
  if (tm->addPassesToEmitFile(pass, dest, nullptr,
                              llvm::CodeGenFileType::ObjectFile))
    throw std::runtime_error("TargetMachine can't emit an object file");
  pass.run(m);
  dest.flush();
}

void build_command::execute_build(bool debug) const {
  std::cout << "Execute build from : " << entry_point_ << "\n";

  auto sources = collect_source_files(source_root_);
  if (sources.empty())
    throw std::runtime_error("no source files found :" + source_root_);

  sema_builder builder;

  // Phase1
  for (const auto& path : sources) {
    std::ifstream fs(path);
    if (!fs.is_open())
      throw std::runtime_error("Can not opened file : " + path);

    antlr4::ANTLRInputStream input(fs);
    ClearLanguageLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    ClearLanguageParser parser(&tokens);
    auto* tree = parser.start();

    builder.collect_signatures(tree);
  }

  // Phase2
  for (const auto& path : sources) {
    std::ifstream fs(path);
    if (!fs.is_open())
      throw std::runtime_error("Can not opened file : " + path);

    antlr4::ANTLRInputStream input(fs);
    ClearLanguageLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    ClearLanguageParser parser(&tokens);
    auto* tree = parser.start();

    builder.construct_target(tree);
  }

  auto mod = builder.take_module();

  if (!entry_point_.empty()) mod->entry_name = entry_point_;

  if (mod->entry_name.empty())
    throw std::runtime_error("entry point not resolved");

  auto ctx = std::make_unique<llvm::LLVMContext>();
  ir_gen_from_sema ir(*ctx, "ClearModule");
  ir.emit_module(*mod);

  if (debug) {
    support_execute_debug(ir, ctx);
    return;
  }

  // ---- AOT: IR -> .obj -> exe/dll ----
  namespace fs = std::filesystem;
  fs::create_directories(output_path_);
  const std::string base = app_name_;
  const auto out_dir = fs::path(output_path_);
  const fs::path obj_path = out_dir / (base + ".obj");
  const bool as_dll = (kind_ == build_kind::dll);
  const fs::path out_path = out_dir / (base + (as_dll ? ".dll" : ".exe"));

  {
    auto& m = ir.module();
    emit_object_file(m, obj_path.string());
  }

#ifdef HAVE_LLD
  link_with_lld(obj_path.string(), out_path.string(), as_dll);
#else
  link_with_system_linker(obj_path.string(), out_path.string(), as_dll);
#endif

  std::cout << "Built: " << out_path.string() << "\n";
}
