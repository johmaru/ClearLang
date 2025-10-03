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

    sema_builder sb;
    sb.visit(tree);
    const auto mod = sb.take_module();

    auto ctx = std::make_unique<llvm::LLVMContext>();
    ir_gen_from_sema ir(*ctx, "ClearModule");
    ir.emit_module(*mod);
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

int support_execute_debug(ir_gen_from_sema& ir, std::unique_ptr<llvm::LLVMContext>& ctx) {
    std::cout << "=== Executing ===" << '\n';
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::unique_ptr<llvm::Module> m = ir.take_module();
    llvm::orc::ThreadSafeModule tsm(std::move(m), std::move(ctx));

    const auto j = llvm::cantFail(llvm::orc::LLJITBuilder().create());
    auto& jd = j->getMainJITDylib();

    // llvm version gap
#if defined(__APPLE__)
    constexpr char global_prefix = '_';
#else
    constexpr char global_prefix = '\0';
#endif

    jd.addGenerator(llvm::cantFail(
        llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(global_prefix)));
    llvm::cantFail(j->addIRModule(std::move(tsm)));


    const auto addr = llvm::cantFail(j->lookup("__cl_entry"));
    using entry_fn = int32_t(*)(uint8_t*, int32_t);
    const entry_fn run = addr.toPtr<entry_fn>();

    uint8_t buf[16] = { 0 };
    if (const int32_t written = run(buf, static_cast<int32_t>(sizeof(buf))); written <= 0) {
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

    if (auto ec_sym_or_err = j->lookup("__cl_exit_code")) {
        const auto ec_sym = *ec_sym_or_err;
        using exit_code_fn = int32_t(*)();
        const exit_code_fn exit_code = ec_sym.toPtr<exit_code_fn>();
        std::cout << "exit=" << exit_code() << "\n";
        return exit_code();
    }
    return 0;
}
