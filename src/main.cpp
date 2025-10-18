#include <llvm/ExecutionEngine/Orc/LLJIT.h>

#include <llvm/Support/TargetSelect.h>

#include <llvm/Support/raw_ostream.h>

#include <llvm/Transforms/Utils/Cloning.h>

#include <ConsoleErrorListener.h>

#include <antlr4-runtime.h>


#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>

#include "commands/BuildCommand.h"

// ir debugging, if return os error code
#include <atomic>
#include <cstdint>
static std::atomic<int32_t> g_exit_code{0};
extern "C" void cl_set_exit_code(const int32_t c) {
  g_exit_code.store(c, std::memory_order_relaxed);
}
extern "C" int32_t cl_exit_code() {
  return g_exit_code.load(std::memory_order_relaxed);
}

#ifdef _WIN32
#include <Windows.h>
extern "C" void cl_set_exit_code_from_win32_last_error() {
  const DWORD e = GetLastError();
  g_exit_code.store(e ? static_cast<int32_t>(e) : 1, std::memory_order_relaxed);
}
extern "C" void cl_force_win32_error(const DWORD code) {
  SetLastError(code);
  cl_set_exit_code_from_win32_last_error();
}
#endif

int main(int argc, const char* argv[]) {
  try {
    if (argc < 2) {
      std::cerr << "usage: clr <command> [--debug]\n";
      return 1;
    }
    const std::string cmd = argv[1];
    bool debug = false;
    for (int i = 2; i < argc; ++i) {
      if (std::string(argv[i]) == "--debug") debug = true;
    }

    if (cmd == "build") {
      const build_command bc("build.clr");
      bc.execute_build(debug);
    } else if (cmd == "init") {
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
      std::cerr << "unsupported command: " << cmd << "\n";
      return 1;
    }
    return 0;
  } catch (const std::exception& ex) {
    std::cerr << "error: " << ex.what() << '\n';
    return 1;
  }
}