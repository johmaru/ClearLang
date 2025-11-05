#include "BuildCommand.h"

#include "../llvm/Executer.h"
#include "../sema/SemaBuilder.h"
#include "ClearLanguageLexer.h"

#include <antlr4-runtime.h>
#include <filesystem>
#include <fstream>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <system_error>

#ifdef HAVE_LLD
#include <lld/Common/Driver.h> // lldMain, DriverDef
#if defined(_WIN32)

namespace lld::coff {
bool link(llvm::ArrayRef<const char*>, llvm::raw_ostream&, llvm::raw_ostream&, bool /*ExitEarly*/,
          bool /*DisableOutput*/);
} // namespace lld::coff

#endif
#endif

#ifdef HAVE_LLD
static void linkWithLld(const std::string& obj_path, const std::string& out_path,
                        const bool AS_DLL) {
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
    if (AS_DLL) {
        argv.push_back("/dll");
    } else {
        argv.push_back("/subsystem:console");
    }

#ifdef CLEAR_RUNTIME_LIB_DIR
#ifdef CLEAR_RUNTIME_LIB_NAME
    const std::string RTLIB = std::string(CLEAR_RUNTIME_LIB_DIR) + "/" + CLEAR_RUNTIME_LIB_NAME;
    argv.push_back(add(RTLIB));
#endif
#endif

#ifdef CLEAR_AOT_ENTRY_LIB_DIR
#ifdef CLEAR_AOT_ENTRY_LIB_NAME
    const std::string ENTRYLIB =
        std::string(CLEAR_AOT_ENTRY_LIB_DIR) + "/" + CLEAR_AOT_ENTRY_LIB_NAME;
    argv.push_back(add(ENTRYLIB));
#endif
#endif

    argv.push_back(add(obj_path));

    const lld::DriverDef DRIVERS_ARR[] = {{lld::Flavor::WinLink, &lld::coff::link}};
    const auto DRIVERS = llvm::ArrayRef<lld::DriverDef>(DRIVERS_ARR, 1);

    auto result = lld::lldMain(argv, llvm::outs(), llvm::errs(), DRIVERS);
    if (result.retCode != 0) {
        throw std::runtime_error("lld::lldMain (COFF) failed");
    }
#endif
}
#endif // HAVE_LLD

BuildCommand::BuildCommand(std::string build_script_path)
    : build_script_path_(std::move(build_script_path)) {
    parseBuildScript();
}

void BuildCommand::parseBuildScript() {
    std::ifstream file(build_script_path_);
    if (!file.is_open()) {
        throw std::runtime_error("build.clr not found");
    }

    antlr4::ANTLRInputStream input(file);
    ClearLanguageLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    ClearLanguageParser parser(&tokens);

    auto* tree = parser.start();

    SemaBuilder s_builder;

    s_builder.collectSignatures(tree);
    s_builder.constructTarget(tree);

    auto mod = s_builder.takeModule();
    extractBuildFunctions(*mod);
}

void BuildCommand::extractBuildFunctions(const sema::Module& mod) {
    for (const auto& func : mod.functions) {
        const bool IS_CONFIG =
            func->name == "configure" || func->name == "build::configure" ||
            (func->name.size() > 11 && func->name.substr(func->name.size() - 10) == "::configure");

        if (!IS_CONFIG) {
            continue;
        }

        for (const auto& stmt : func->body->statements) {
            if (const auto* expr_stmt = dynamic_cast<sema::StmtExpr*>(stmt.get())) {
                if (const auto* call = dynamic_cast<sema::Call*>(expr_stmt->expr.get())) {
                    if (call->callee == "__set_entry") {
                        if (const auto* lit = dynamic_cast<sema::Literal*>(call->args[0].get())) {
                            entry_point_ = asString(lit->value);
                        }
                    }
                    if (call->callee == "__add_source") {
                        if (const auto* lit = dynamic_cast<sema::Literal*>(call->args[0].get())) {
                            source_root_ = asString(lit->value);
                        }
                    }
                    if (call->callee == "__set_output") {
                        if (const auto* lit = dynamic_cast<sema::Literal*>(call->args[0].get())) {
                            output_path_ = asString(lit->value);
                        }
                    }
                    if (call->callee == "__set_target") {
                        if (const auto* lit = dynamic_cast<sema::Literal*>(call->args[0].get())) {
                            const auto k = asString(lit->value);
                            if (k == "exe" || k == "executable") {
                                kind_ = build_kind::EXE;
                            } else if (k == "dll" || k == "shared") {
                                kind_ = build_kind::DLL;
                            } else if (k == "none") {
                                kind_ = build_kind::NONE;
                            } else {
                                throw std::runtime_error("unknown target kind: " + k);
                            }
                        }
                    }
                    if (call->callee == "__set_app_name") {
                        if (const auto* lit = dynamic_cast<sema::Literal*>(call->args[0].get())) {
                            app_name_ = asString(lit->value);
                        }
                    }
                }
            }
        }
    }
}

std::vector<std::string> BuildCommand::collectSourceFiles(const std::string& root) {
    std::vector<std::string> files;
    for (const auto& dir_entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!dir_entry.is_regular_file()) {
            continue;
        }
        if (dir_entry.path().extension() == ".clr" && dir_entry.path().filename() != "build.clr") {
            files.push_back(dir_entry.path().string());
        }
    }
    return files;
}

static void emitObjectFile(llvm::Module& module, const std::string& obj_path) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    const std::string TRIPLE = llvm::sys::getDefaultTargetTriple();
    module.setTargetTriple(TRIPLE);

    std::string err;
    const llvm::Target* target = llvm::TargetRegistry::lookupTarget(TRIPLE, err);
    if (target == nullptr) {
        throw std::runtime_error("lookupTarget failed: " + err);
    }

    llvm::TargetOptions opt;
    std::unique_ptr<llvm::TargetMachine> t_m(
        target->createTargetMachine(TRIPLE, "generic", "", opt, std::nullopt));

    if (!t_m) {
        throw std::runtime_error("createTargetMachine failed");
    }

    module.setDataLayout(t_m->createDataLayout());

    std::error_code error_code;
    llvm::raw_fd_ostream dest(obj_path, error_code, llvm::sys::fs::OF_None);
    if (error_code) {
        throw std::runtime_error("open object file failed: " + error_code.message());
    }

    llvm::legacy::PassManager pass;
    if (t_m->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::ObjectFile)) {
        throw std::runtime_error("TargetMachine can't emit an object file");
    }
    pass.run(module);
    dest.flush();
}

void BuildCommand::executeBuild(bool debug) const {
    std::cout << "Execute build from : " << entry_point_ << "\n";

    auto sources = collectSourceFiles(source_root_);
    if (sources.empty()) {
        throw std::runtime_error("no source files found :" + source_root_);
    }

    SemaBuilder builder;

    // Phase1
    for (const auto& path : sources) {
        std::ifstream ifs(path);
        if (!ifs.is_open()) {
            throw std::runtime_error("Can not opened file : " + path);
        }

        antlr4::ANTLRInputStream input(ifs);
        ClearLanguageLexer lexer(&input);
        antlr4::CommonTokenStream tokens(&lexer);
        ClearLanguageParser parser(&tokens);
        auto* tree = parser.start();

        builder.collectSignatures(tree);
    }

    // Phase2
    for (const auto& path : sources) {
        std::ifstream ifs(path);
        if (!ifs.is_open()) {
            throw std::runtime_error("Can not opened file : " + path);
        }

        antlr4::ANTLRInputStream input(ifs);
        ClearLanguageLexer lexer(&input);
        antlr4::CommonTokenStream tokens(&lexer);
        ClearLanguageParser parser(&tokens);
        auto* tree = parser.start();

        builder.constructTarget(tree);
    }

    auto mod = builder.takeModule();

    if (!entry_point_.empty()) {
        mod->entry_name = entry_point_;
    }

    if (mod->entry_name.empty()) {
        throw std::runtime_error("entry point not resolved");
    }

    auto ctx = std::make_unique<llvm::LLVMContext>();
    IrGenFromSema ir(*ctx, "ClearModule");
    ir.emitModule(*mod);

    if (debug) {
        supportExecuteDebug(ir, ctx);
        return;
    }

    // ---- AOT: IR -> .obj -> exe/dll ----
    namespace fs = std::filesystem;
    fs::create_directories(output_path_);
    const std::string BASE = app_name_;
    const auto OUT_DIR = fs::path(output_path_);
    const fs::path OBJ_PATH = OUT_DIR / (BASE + ".obj");
    const bool AS_DLL = (kind_ == build_kind::DLL);
    const fs::path OUT_PATH = OUT_DIR / (BASE + (AS_DLL ? ".dll" : ".exe"));

    {
        auto& m = ir.module();
        emitObjectFile(m, OBJ_PATH.string());
    }

#ifdef HAVE_LLD
    linkWithLld(OBJ_PATH.string(), OUT_PATH.string(), AS_DLL);
#else
    linkWithSystemLinker(OBJ_PATH.string(), OUT_PATH.string(), AS_DLL);
#endif

    std::cout << "Built: " << OUT_PATH.string() << "\n";
}
