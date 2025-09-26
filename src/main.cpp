#include <ConsoleErrorListener.h>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <string>
#include "ClearLanguageLexer.h"
#include "ClearLanguageParser.h"
#include "sema/SemaBuilder.h"
#include "ir/IRGenFromSema.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include "core/CLType.h"
#include <antlr4-runtime.h>
#include "llvm/Executer.h"

// ir debugging, if return os error code
#include <atomic>
#include <cstdint>
static std::atomic<int32_t> g_exit_code{0};
extern "C" void cl_set_exit_code(const int32_t c){ g_exit_code.store(c, std::memory_order_relaxed); }
extern "C" int32_t cl_exit_code(){ return g_exit_code.load(std::memory_order_relaxed); }

#ifdef _WIN32
#include <Windows.h>
extern "C" void cl_set_exit_code_from_win32_last_error(){
    const DWORD e = GetLastError();
    g_exit_code.store(e ? static_cast<int32_t>(e) : 1, std::memory_order_relaxed);
}
extern "C" void cl_force_win32_error(const DWORD code){
    SetLastError(code);
    cl_set_exit_code_from_win32_last_error();
}
#endif

int main(int argc, const char* argv[]) {
     try {
        std::string code;

        std::filesystem::path base = std::filesystem::path(argv[0]).parent_path();
        std::filesystem::path src_path = base / "main.clr";

        auto read_file = [](const std::filesystem::path& p) {
            std::ifstream ifs(p, std::ios::binary);
            return std::string((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
        };

        if (std::filesystem::exists(src_path)) {
            code = read_file(src_path);
        } else if (argc >= 3) {
	        if (std::filesystem::path arg_path = argv[2]; std::filesystem::exists(arg_path)) {
                code = read_file(arg_path);
            } else {
                code = argv[2];
            }
        } else {
            return -1;
        }

        antlr4::ANTLRInputStream input(code);
        ClearLanguageLexer lexer(&input);
        antlr4::CommonTokenStream tokens(&lexer);
        ClearLanguageParser parser(&tokens);

        if (execute(argc, argv, parser) != 0) {
			throw std::runtime_error("Execution failed");
        }
        
        return 0;
    } catch (const std::exception& ex) {
        std::cerr << "error: " << ex.what() << '\n';
        return 1;
    }
}