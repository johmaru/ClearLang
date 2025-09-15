#include <iostream>
#include <fstream>
#include <filesystem>
#include <string>
#include "ClearLanguageLexer.h"
#include "ClearLanguageParser.h"
#include "sema/SemaBuilder.h"
#include "ast/CLVisitor.cpp"
#include "ir/IRGenFromSema.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include "core/CLType.h"

// ir debugging, if return os error code
#include <atomic>
#include <cstdint>
static std::atomic<int32_t> g_exitCode{0};
extern "C" void __cl_set_exit_code(int32_t c){ g_exitCode.store(c, std::memory_order_relaxed); }
extern "C" int32_t __cl_exit_code(){ return g_exitCode.load(std::memory_order_relaxed); }

#ifdef _WIN32
#include <Windows.h>
extern "C" void __cl_set_exit_code_from_win32_last_error(){
    DWORD e = GetLastError();
    g_exitCode.store(e ? (int32_t)e : 1, std::memory_order_relaxed);
}
extern "C" void __cl_force_win32_error(DWORD code){
    SetLastError(code);
    __cl_set_exit_code_from_win32_last_error();
}
#endif

int main(int argc, const char* argv[]) {
     try {
        std::string code;

        std::filesystem::path base = std::filesystem::path(argv[0]).parent_path();
        std::filesystem::path srcPath = base / "src" / "main.clr";

        auto readFile = [](const std::filesystem::path& p) {
            std::ifstream ifs(p, std::ios::binary);
            return std::string((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
        };

        if (std::filesystem::exists(srcPath)) {
            code = readFile(srcPath);
        } else if (argc >= 3) {
            std::filesystem::path argPath = argv[2];
            if (std::filesystem::exists(argPath)) {
                code = readFile(argPath);
            } else {
                code = argv[2];
            }
        } else {
            code.assign((std::istreambuf_iterator<char>(std::cin)), std::istreambuf_iterator<char>());
        }

        ANTLRInputStream input(code);
        ClearLanguageLexer lexer(&input);
        CommonTokenStream tokens(&lexer);
        ClearLanguageParser parser(&tokens);
        auto* tree = parser.start();

        SemaBuilder sb;
        sb.visit(tree);
        auto mod = sb.takeModule();

        auto ctx = std::make_unique<llvm::LLVMContext>();
        IRGenFromSema ir(*ctx, "ClearModule");
        ir.emitModule(*mod);
        if (argc >= 2 && std::string(argv[1]) == "--emit-llvm") {
            ir.module().print(llvm::outs(), nullptr);
            return 0;
        }

        if (argc >= 2 && std::string(argv[1]) == "--debug-print") {
            std::cout << "=== Executing ===" << std::endl;
            llvm::InitializeNativeTarget();
            llvm::InitializeNativeTargetAsmPrinter();
            llvm::InitializeNativeTargetAsmParser();

            std::unique_ptr<llvm::Module> M = ir.takeModule();
            llvm::orc::ThreadSafeModule TSM(std::move(M), std::move(ctx));

            auto J = llvm::cantFail(llvm::orc::LLJITBuilder().create());
            auto &JD = J->getMainJITDylib();
            
            // llvm version gap
            #if defined(__APPLE__)
                char GlobalPrefix = '_';
            #else
                char GlobalPrefix = '\0';
            #endif

            JD.addGenerator(llvm::cantFail(
                llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(GlobalPrefix)));
            llvm::cantFail(J->addIRModule(std::move(TSM)));


            auto addr = llvm::cantFail(J->lookup("__cl_entry"));
            using EntryFn = int32_t (*)(uint8_t*, int32_t);
            EntryFn run = addr.toPtr<EntryFn>();

            uint8_t buf[16] = {0};
            int32_t written = run(buf, (int32_t)sizeof(buf));
            if (written <= 0) {
                std::cout << "no result\n";
            } else {
                uint8_t tag = buf[0];
                if (tag == 1) {
                    std::cout << "()\n";
                } else if (tag == 2){
                    int8_t v = *(int8_t*)&buf[1];
                    std::cout << (int)v << "\n";
                } else if (tag == 3){
                    int32_t v = *(int32_t*)&buf[1];
                    std::cout << v << "\n";
                } else if (tag == 4) {
                    uint8_t v = *(uint8_t*)&buf[1];
                    std::cout << (int)v << "\n";
                } else if (tag == 5) {
                    uint32_t v = *(uint32_t*)&buf[1];
                    std::cout << v << "\n";
                } else if (tag == 6) {
                    uint16_t bits = (uint16_t)buf[1] | ((uint16_t)buf[2] << 8);
                    CLHalf h; h.bits = bits;
                    float f = static_cast<float>(h);
                    std::cout << f << "\n";
                } else {
                    std::cout << "unsupported tag: " << (int)tag
                            << ", bytes=" << written << "\n";
                }
            }

            if (auto ecSymOrErr = J->lookup("__cl_exit_code")) {
                auto ecSym = *ecSymOrErr;
                using ExitCodeFn = int32_t (*)();
                ExitCodeFn exitCode = ecSym.toPtr<ExitCodeFn>();
                std::cout << "exit=" << exitCode() << "\n";
                return exitCode();
            }
            return 0;
        }
        
        return 0;
    } catch (const std::exception& ex) {
        std::cerr << "error: " << ex.what() << std::endl;
        return 1;
    }
}