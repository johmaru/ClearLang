#include "Executer.h"
#include <antlr4-runtime.h> 

int execute(int argc, const char* argv[], ClearLanguageParser& parser) {
    parser.removeErrorListeners();
    parser.addErrorListener(&antlr4::ConsoleErrorListener::INSTANCE);
    parser.setErrorHandler(std::make_shared<antlr4::BailErrorStrategy>());

    auto* tree = parser.start();
    if (parser.getNumberOfSyntaxErrors() > 0) {
        std::cerr << "syntax errors: " << parser.getNumberOfSyntaxErrors() << "\n";
        return 2;
    }

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
        if (support_execute_debug(ir, ctx) != 0) {
            return 3;
		}
    }
    return 0;
}

int support_execute_debug(IRGenFromSema& ir, std::unique_ptr<llvm::LLVMContext>& ctx) {
    std::cout << "=== Executing ===" << '\n';
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::unique_ptr<llvm::Module> m = ir.takeModule();
    llvm::orc::ThreadSafeModule tsm(std::move(m), std::move(ctx));

    auto j = llvm::cantFail(llvm::orc::LLJITBuilder().create());
    auto& jd = j->getMainJITDylib();

    // llvm version gap
#if defined(__APPLE__)
    char GlobalPrefix = '_';
#else
    char global_prefix = '\0';
#endif

    jd.addGenerator(llvm::cantFail(
        llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(global_prefix)));
    llvm::cantFail(j->addIRModule(std::move(tsm)));


    auto addr = llvm::cantFail(j->lookup("__cl_entry"));
    using entry_fn = int32_t(*)(uint8_t*, int32_t);
    entry_fn run = addr.toPtr<entry_fn>();

    uint8_t buf[16] = { 0 };
    if (int32_t written = run(buf, static_cast<int32_t>(sizeof(buf))); written <= 0) {
        std::cout << "no result\n";
    }
    else {
        /*
        uint8_t tag = buf[0];
        if (tag == 1) {
            std::cout << "()\n";
        }
        else if (tag == 2) {
            int8_t v = *reinterpret_cast<int8_t*>(&buf[1]);
            std::cout << static_cast<int>(v) << "\n";
        }
        else if (tag == 3) {
            int16_t v = *reinterpret_cast<int16_t*>(&buf[1]);
            std::cout << v << "\n";
        }
        else if (tag == 4) {
            int32_t v = *reinterpret_cast<int32_t*>(&buf[1]);
            std::cout << v << "\n";
        }
        else if (tag == 5) {
            int64_t v = *reinterpret_cast<int64_t*>(&buf[1]);
            std::cout << v << "\n";
        }
        else if (tag == 6) {
            uint16_t bits = static_cast<uint16_t>(buf[1]) | (static_cast<uint16_t>(buf[2]) << 8);
            CLHalf h; h.bits = bits;
            float f = static_cast<float>(h);
            std::cout << f << "\n";
        }
        else {
            std::cout << "unsupported tag: " << static_cast<int>(tag)
                << ", bytes=" << written << "\n";
        }
        */
    }

    if (auto ecSymOrErr = j->lookup("__cl_exit_code")) {
        auto ecSym = *ecSymOrErr;
        using ExitCodeFn = int32_t(*)();
        ExitCodeFn exitCode = ecSym.toPtr<ExitCodeFn>();
        std::cout << "exit=" << exitCode() << "\n";
        return exitCode();
    }
    return 0;
}
