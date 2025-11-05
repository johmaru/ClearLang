#include "Executer.h"

#include <antlr4-runtime.h>

int execute(const int argc, const char* argv[], ClearLanguageParser& parser) {
    parser.removeErrorListeners();
    parser.addErrorListener(&antlr4::ConsoleErrorListener::INSTANCE);
    parser.setErrorHandler(std::make_shared<antlr4::BailErrorStrategy>());

    auto* tree = parser.start();
    if (parser.getNumberOfSyntaxErrors() > 0) {
        std::cerr << "syntax errors: " << parser.getNumberOfSyntaxErrors() << "\n";
        return 2;
    }

    SemaBuilder s_builder;
    s_builder.visit(tree);
    const auto MOD = s_builder.takeModule();

    auto ctx = std::make_unique<llvm::LLVMContext>();
    IrGenFromSema ir(*ctx, "ClearModule");
    ir.emitModule(*MOD);
    if (argc >= 2 && std::string(argv[1]) == "--emit-llvm") {
        ir.module().print(llvm::outs(), nullptr);
        return 0;
    }

    if (argc >= 2 && std::string(argv[1]) == "--debug-print") {
        if (supportExecuteDebug(ir, ctx) != 0) {
            return 3;
        }
    }
    return 0;
}

int supportExecuteDebug(IrGenFromSema& ir, std::unique_ptr<llvm::LLVMContext>& ctx) {
    std::cout << "=== Executing ===" << '\n';
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::unique_ptr<llvm::Module> module = ir.takeModule();
    llvm::orc::ThreadSafeModule tsm(std::move(module), std::move(ctx));

    const auto JIT = llvm::cantFail(llvm::orc::LLJITBuilder().create());
    auto& jitdylib = JIT->getMainJITDylib();

    // llvm version gap
#if defined(__APPLE__)
    constexpr char GLOBAL_PREFIX = '_';
#else
    constexpr char GLOBAL_PREFIX = '\0';
#endif

    jitdylib.addGenerator(llvm::cantFail(
        llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(GLOBAL_PREFIX)));
    llvm::cantFail(JIT->addIRModule(std::move(tsm)));

    const auto ADDR = llvm::cantFail(JIT->lookup("__cl_entry"));
    using entry_fn = int32_t (*)(uint8_t*, int32_t);
    const entry_fn RUN = ADDR.toPtr<entry_fn>();

    uint8_t buf[16] = {0};
    if (const int32_t WRITTEN = RUN(buf, static_cast<int32_t>(sizeof(buf))); WRITTEN <= 0) {
        std::cout << "no result\n";
    } else {
        /*
        uint8_t tag = buf[0];
        if (tag == 1) {
            std::cout << "()\n";
        }
        else if (tag == 2) {
            int8_t val = *reinterpret_cast<int8_t*>(&buf[1]);
            std::cout << static_cast<int>(val) << "\n";
        }
        else if (tag == 3) {
            int16_t val = *reinterpret_cast<int16_t*>(&buf[1]);
            std::cout << val << "\n";
        }
        else if (tag == 4) {
            int32_t val = *reinterpret_cast<int32_t*>(&buf[1]);
            std::cout << val << "\n";
        }
        else if (tag == 5) {
            int64_t val = *reinterpret_cast<int64_t*>(&buf[1]);
            std::cout << val << "\n";
        }
        else if (tag == 6) {
            uint16_t bits = static_cast<uint16_t>(buf[1]) | (static_cast<uint16_t>(buf[2]) << 8);
            CLHalf half; half.bits = bits;
            float float_val = static_cast<float>(half);
            std::cout << float_val << "\n";
        }
        else {
            std::cout << "unsupported tag: " << static_cast<int>(tag)
                << ", bytes=" << WRITTEN << "\n";
        }
        */
    }

    if (auto ec_sym_or_err = JIT->lookup("__cl_exit_code")) {
        const auto EC_SYM = *ec_sym_or_err;
        using exit_code_fn = int32_t (*)();
        const exit_code_fn EXIT_CODE = EC_SYM.toPtr<exit_code_fn>();
        std::cout << "exit=" << EXIT_CODE() << "\n";
        return EXIT_CODE();
    }
    return 0;
}
