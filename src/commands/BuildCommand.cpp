#include "BuildCommand.h"
#include <antlr4-runtime.h>

#include "ClearLanguageLexer.h"
#include "../sema/SemaBuilder.h"
#include "../llvm/Executer.h"
#include <filesystem>

build_command::build_command(std::string build_script_path) : build_script_path_(std::move(build_script_path)) {
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
			func->name == "configure" ||
			func->name == "build::configure" ||
			(func->name.size() > 11 && func->name.substr(func->name.size() - 10) == "::configure");

		if (!is_config) continue;
		
			for (const auto& stmt : func->body->statements) {
				if (const auto* expr_stmt = dynamic_cast<sema::stmt_expr*>(stmt.get())) {
					if (const auto* call = dynamic_cast<sema::call*>(expr_stmt->expr.get())) {
						if (call->callee == "__set_entry") {
							if (const auto* lit = dynamic_cast<sema::literal*>(call->args[0].get())) {
								entry_point_ = as_string(lit->value);
							}
						}
						if (call->callee == "__add_source") {
							if (const auto* lit = dynamic_cast<sema::literal*>(call->args[0].get())) {
								source_root_ = as_string(lit->value);
							}
						}
						if (call->callee == "__set_output") {
							if (const auto* lit = dynamic_cast<sema::literal*>(call->args[0].get())) {
								output_path_ = as_string(lit->value);
							}
						}
					}
				}
			}
	}
}

std::vector<std::string> build_command::collect_source_files(const std::string& root) {
	std::vector<std::string> files;
	for (auto& p : std::filesystem::recursive_directory_iterator(root)) {
		if (!p.is_regular_file()) continue;
		if (p.path().extension() == ".clr" && p.path().filename() != "build.clr")
			files.push_back(p.path().string());
	}
	return files;
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

	if (!entry_point_.empty())
		mod->entry_name = entry_point_;

	if (mod->entry_name.empty())
		throw std::runtime_error("entry point not resolved");

	auto ctx = std::make_unique<llvm::LLVMContext>();
	ir_gen_from_sema ir(*ctx, "ClearModule");
	ir.emit_module(*mod);

	if (debug) {
		support_execute_debug(ir, ctx);
		return;
	}
}

