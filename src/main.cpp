#include "commands/BuildCommand.h"

#include <ConsoleErrorListener.h>
#include <antlr4-runtime.h>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <string>

// ir debugging, if return os error code
#include <atomic>
#include <cstdint>
static std::atomic<int32_t> g_exit_code{0};
extern "C" void clSetExitCode(const int32_t CODE) {
    g_exit_code.store(CODE, std::memory_order_relaxed);
}
extern "C" int32_t clExitCode() {
    return g_exit_code.load(std::memory_order_relaxed);
}

#ifdef _WIN32
#include <Windows.h>
extern "C" void clSetExitCodeFromWin32LastError() {
    const DWORD WHAT = GetLastError();
    g_exit_code.store((WHAT != 0U) ? static_cast<int32_t>(WHAT) : 1, std::memory_order_relaxed);
}
extern "C" void clForceWin32Error(const DWORD CODE) {
    SetLastError(CODE);
    clSetExitCodeFromWin32LastError();
}
#endif

int main(int argc, const char* argv[]) {
    try {
        if (argc < 2) {
            std::cerr << "usage: clr <command> [--debug]\n";
            return 1;
        }

        const std::string CMD = argv[1];
        bool debug = false;
        for (int i = 2; i < argc; ++i) {
            if (std::string(argv[i]) == "--debug") {
                debug = true;
            }
        }

        if (CMD == "build") {
            const BuildCommand BCOMMAND("build.clr");
            BCOMMAND.executeBuild(debug);
        } else if (CMD == "init") {
            try {
                std::ofstream build_file("build.clr");
                std::string default_build_text = R"(package build; 
				func configure() -> unit {
				  __set_entry("main::main"); 
				  __add_source("src");
				  __set_output("build / bin");
				})";

                build_file << default_build_text << '\n';

                std::filesystem::path init_path = std::filesystem::current_path();

                std::filesystem::create_directory("src");

                std::filesystem::current_path(init_path / "src");

                std::ofstream src_file("main.clr");
                std::string default_main_text = R"(package main;
				[EntryPoint]
				func main() -> () {
				    a: u8 = 50;
				    b: string = "30";
					c: bool = true;
					d: f32 = 3.5;
				    e: u8 = if_test(a, b as! u8, c);
					
					__cl_u8_printfn(e);
					__cl_f32_printfn(d);
				}

				func if_test(a:u8, b:u8, c:bool) -> u8 {
				    if (c) {
						return a + b;
					} else {
						return a - b;
					}

				})";

                src_file << default_main_text << '\n';
                std::cout << "Initialize Completed" << '\n';

            } catch (const std::filesystem::filesystem_error& e) {
                std::cerr << "Initialize Error : " << e.what() << '\n';
                return 1;
            }

        } else {
            std::cerr << "unsupported command: " << CMD << "\n";
            return 1;
        }
        return 0;
    } catch (const std::exception& ex) {
        std::cerr << "error: " << ex.what() << '\n';
        return 1;
    }
}