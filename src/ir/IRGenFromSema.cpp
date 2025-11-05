#include "IRGenFromSema.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/ErrorHandling.h"

#include <cstdint>
#include <llvm/ADT/APFloat.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Verifier.h>

IrGenFromSema::IrGenFromSema(llvm::LLVMContext& ctx, const std::string& module_name)
    : ctx_(ctx), mod_(std::make_unique<llvm::Module>(module_name, ctx_)),
      builder_(std::make_unique<llvm::IRBuilder<>>(ctx_)) {}

llvm::Module& IrGenFromSema::module() const {
    return *mod_;
}

void IrGenFromSema::emitModule(const sema::Module& module) {
    // Predeclare all functions with proper signatures
    for (const auto& func : module.functions) {
        std::vector<llvm::Type*> param_tys;
        for (auto& param : func->params) {
            param_tys.push_back(toLlvmType(param.type));
        }
        auto* ret_ty = toLlvmType(func->return_type);
        auto* fn_ty = llvm::FunctionType::get(ret_ty, param_tys, false);
        (void) mod_->getOrInsertFunction(func->name, fn_ty);
    }
    // Then emit bodies
    for (const auto& func : module.functions) {
        emitFunction(*func);
    }
    emitEntryShim(module);

    // Exit code
    if (mod_->getFunction("__cl_exit_code") == nullptr) {
        auto* i32_ty = llvm::Type::getInt32Ty(ctx_);
        auto* fn_ty = llvm::FunctionType::get(i32_ty, {}, false);
        auto* func = llvm::Function::Create(fn_ty, llvm::Function::ExternalLinkage,
                                            "__cl_exit_code", mod_.get());
        auto* b_blk = llvm::BasicBlock::Create(ctx_, "entry", func);
        builder_->SetInsertPoint(b_blk);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 0));
    }
}

std::unique_ptr<llvm::Module> IrGenFromSema::takeModule() {
    return std::move(mod_);
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
llvm::AllocaInst* IrGenFromSema::createAllocaInEntry(llvm::Function* func, llvm::Type* type,
                                                     llvm::StringRef name) {
    auto& entry = func->getEntryBlock();
    llvm::IRBuilder<> tmp_builder(&entry, entry.begin());
    return tmp_builder.CreateAlloca(type, nullptr, name);
}

void IrGenFromSema::emitEntryShim(const sema::Module& module) const {
    if (module.entry_name.empty()) {
        return; // no entry point
    }
    llvm::Function* entry = mod_->getFunction(module.entry_name);
    if (entry == nullptr) {
        return;
    }
    if (entry->arg_size() != 0) {
        return;
    }

    auto& ctx = ctx_;
    auto* i8_ty = llvm::Type::getInt8Ty(ctx);
    auto* i16_ty = llvm::Type::getInt16Ty(ctx);
    auto* i32_ty = llvm::Type::getInt32Ty(ctx);
    auto* i8_ptr_ty = llvm::Type::getInt8Ty(ctx)->getPointerTo();

    auto* fn_ty = llvm::FunctionType::get(i32_ty, {i8_ptr_ty, i32_ty}, false);
    auto* func =
        llvm::Function::Create(fn_ty, llvm::Function::ExternalLinkage, "__cl_entry", mod_.get());
    auto* b_blk = llvm::BasicBlock::Create(ctx, "entry", func);
    builder_->SetInsertPoint(b_blk);

    auto* arg = func->arg_begin();
    llvm::Value* out = arg++;

    auto store_tag = [&](const uint8_t TAG) {
        builder_->CreateStore(llvm::ConstantInt::get(i8_ty, TAG), out);
    };

    auto* data_ptr = builder_->CreateGEP(i8_ty, out, llvm::ConstantInt::get(i32_ty, 1));

    const llvm::Type* ret_ty = entry->getReturnType();
    if (ret_ty->isVoidTy()) {
        builder_->CreateCall(entry, {});
        // unit -> tag=1, len=1
        store_tag(1);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 1));
        return;
    }
    if (ret_ty->isIntegerTy(8)) {
        llvm::Value* value = builder_->CreateCall(entry, {});
        store_tag(2);
        auto* p_8 = builder_->CreateBitCast(data_ptr, i8_ty->getPointerTo());
        builder_->CreateStore(value, p_8);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 2)); // 1(tag) + 1 (data)
        return;
    }
    if (ret_ty->isIntegerTy(16)) {
        llvm::Value* value = builder_->CreateCall(entry, {});
        store_tag(3);
        auto* p_16 = builder_->CreateBitCast(data_ptr, i16_ty->getPointerTo());
        builder_->CreateStore(value, p_16);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 3)); // 1(tag) + 2 (data)
        return;
    }
    if (ret_ty->isIntegerTy(32)) {
        llvm::Value* value = builder_->CreateCall(entry, {});
        store_tag(4);
        auto* p32 = builder_->CreateBitCast(data_ptr, i32_ty->getPointerTo());
        builder_->CreateStore(value, p32);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 5)); // 1(tag) + 4 (data)
        return;
    }
    if (ret_ty->isIntegerTy(64)) {
        llvm::Value* value = builder_->CreateCall(entry, {});
        store_tag(5);
        auto* p_64 =
            builder_->CreateBitCast(data_ptr, llvm::Type::getInt64Ty(ctx_)->getPointerTo());
        builder_->CreateStore(value, p_64);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 9)); // 1(tag) + 8 (data)
        return;
    }
    if (ret_ty->isHalfTy()) {
        llvm::Value* half = builder_->CreateCall(entry, {});
        llvm::Value* bits = builder_->CreateBitCast(half, i16_ty);
        store_tag(6);
        auto* p_16 = builder_->CreateBitCast(data_ptr, i16_ty->getPointerTo());
        builder_->CreateStore(bits, p_16);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 3)); // 1(tag) + 2 (data)
        return;
    }
    if (ret_ty->isFloatTy()) {
        llvm::Value* half = builder_->CreateCall(entry, {});
        llvm::Value* bits = builder_->CreateBitCast(half, i32_ty);
        store_tag(7);
        auto* p32 = builder_->CreateBitCast(data_ptr, i32_ty->getPointerTo());
        builder_->CreateStore(bits, p32);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 5));
        return;
    }

    if (ret_ty->isIntegerTy(1)) {
        llvm::Value* value = builder_->CreateCall(entry, {});

        store_tag(8);
        auto* p_8 = builder_->CreateBitCast(data_ptr, i8_ty->getPointerTo());

        value = builder_->CreateZExt(value, i8_ty);
        builder_->CreateStore(value, p_8);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 2));
        return;
    }

    builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 0));
}

llvm::Function* IrGenFromSema::emitFunction(const sema::Function& f) {
    std::vector<llvm::Type*> param_tys;
    param_tys.reserve(f.params.size());
    for (const auto& param : f.params) {
        param_tys.push_back(toLlvmType(param.type));
    }
    auto* ret_ty = toLlvmType(f.return_type);
    auto* fn_ty = llvm::FunctionType::get(ret_ty, param_tys, false);

    // Reuse predeclared function if available
    llvm::Function* func = mod_->getFunction(f.name);
    if (func == nullptr) {
        auto callee = mod_->getOrInsertFunction(f.name, fn_ty);
        func = llvm::cast<llvm::Function>(callee.getCallee());
    }

    unsigned idx = 0;
    for (auto& arg : func->args()) {
        arg.setName(f.params[idx].name);
        ++idx;
    }

    auto* entry = llvm::BasicBlock::Create(ctx_, "entry", func);
    builder_->SetInsertPoint(entry);

    vars_.emplace_back();
    idx = 0;
    for (auto& arg : func->args()) {
        vars_.back()[std::string(arg.getName())] = RValue{&arg};
        ++idx;
    }
    emitBlock(*f.body);

    // Verification currently InsertBlock.
    llvm::BasicBlock* cur = builder_->GetInsertBlock();
    if (cur->getTerminator() == nullptr) {
        if (llvm::pred_empty(cur)) {
            // if unreachable
            builder_->CreateUnreachable();
        } else if (ret_ty->isVoidTy()) {
            builder_->CreateRetVoid();
        } else {
            throw std::runtime_error("missing return");
        }
    }
    vars_.pop_back();
    return func;
}

void IrGenFromSema::emitBlock(const sema::Block& blk) {
    for (const auto& stmt : blk.statements) {
        emitStmt(*stmt);
    }
}

void IrGenFromSema::emitStmt(const sema::Stmt& stmt) {
    if (const auto* stmt_var_decl = dynamic_cast<const sema::StmtVarDecl*>(&stmt)) {
        llvm::Type* type = toLlvmType(stmt_var_decl->decl_type);
        llvm::Value* init =
            stmt_var_decl->init_expr ? emitExpr(*stmt_var_decl->init_expr) : nullptr;

        switch (stmt_var_decl->mut) {
        case sema::mutability::CONST:
        case sema::mutability::LET: {
            if (init == nullptr) {
                init = llvm::UndefValue::get(type);
            }
            vars_.back()[stmt_var_decl->name] = RValue{init};
            break;
        }
        case sema::mutability::VAR: {
            llvm::Function* func = builder_->GetInsertBlock()->getParent();
            auto* addr = createAllocaInEntry(func, type, stmt_var_decl->name);
            if (init == nullptr) {
                init = llvm::Constant::getNullValue(type);
            }
            builder_->CreateStore(init, addr);
            vars_.back()[stmt_var_decl->name] = LValue{addr};
            break;
        }
        }
    } else if (const auto* ret = dynamic_cast<const sema::StmtReturn*>(&stmt)) {
        if (!ret->value) {
            builder_->CreateRetVoid();
        } else {
            auto* value = emitExpr(*ret->value);
            builder_->CreateRet(value);
        }
    } else if (const auto* sif = dynamic_cast<const sema::StmtIf*>(&stmt)) {
        llvm::Value* cond_v = emitExpr(*sif->cond);
        llvm::Value* cond_i1 = nullptr;

        if (cond_v->getType()->isIntegerTy(1)) {
            cond_i1 = cond_v;
        } else if (cond_v->getType()->isIntegerTy()) {
            cond_i1 = builder_->CreateICmpNE(cond_v, llvm::ConstantInt::get(cond_v->getType(), 0));
        } else if (cond_v->getType()->isHalfTy() || cond_v->getType()->isFloatTy()) {
            cond_i1 =
                builder_->CreateFCmpONE(cond_v, llvm::ConstantFP::get(cond_v->getType(), 0.0));
        } else if (cond_v->getType()->isPointerTy()) {
            cond_i1 = builder_->CreateICmpNE(
                cond_v,
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cond_v->getType())));
        } else {
            throw std::runtime_error("if condition: unsupported type");
        }

        llvm::Function* func = builder_->GetInsertBlock()->getParent();
        auto* then_blk = llvm::BasicBlock::Create(ctx_, "if.then", func);
        llvm::BasicBlock* else_blk = nullptr;
        auto* cont_blk = llvm::BasicBlock::Create(ctx_, "if.end", func);

        if (sif->else_blk) {
            else_blk = llvm::BasicBlock::Create(ctx_, "if.else", func);
            builder_->CreateCondBr(cond_i1, then_blk, else_blk);
        } else {
            builder_->CreateCondBr(cond_i1, then_blk, cont_blk);
        }

        builder_->SetInsertPoint(then_blk);
        emitBlock(*sif->then_blk);
        if (builder_->GetInsertBlock()->getTerminator() == nullptr) {
            builder_->CreateBr(cont_blk);
        }

        if (else_blk != nullptr) {
            builder_->SetInsertPoint(else_blk);
            emitBlock(*sif->else_blk);
            if (builder_->GetInsertBlock()->getTerminator() == nullptr) {
                builder_->CreateBr(cont_blk);
            }
        }

        builder_->SetInsertPoint(cont_blk);

    } else if (const auto* stmt_expr = dynamic_cast<const sema::StmtExpr*>(&stmt)) {
        (void) emitExpr(*stmt_expr->expr);
    } else {
    }
}

llvm::Value* IrGenFromSema::emitExpr(const sema::Expr& expr) {
    if (const auto* lit = dynamic_cast<const sema::Literal*>(&expr)) {
        return toLlvmConst(lit->value);
    }
    if (const auto* v_ref = dynamic_cast<const sema::VarRef*>(&expr)) {
        const auto ITER = vars_.back().find(v_ref->name);
        if (ITER == vars_.back().end()) {
            throw std::runtime_error("undefined var in IRGen: " + v_ref->name);
        }
        const Binding& bind = ITER->second;
        if (const auto* ptr = std::get_if<RValue>(&bind)) {
            return ptr->val;
        }
        if (const auto* ptr = std::get_if<LValue>(&bind)) {
            llvm::Type* type = ptr->addr->getAllocatedType();
            return builder_->CreateLoad(type, ptr->addr);
        }
        llvm::report_fatal_error("invalid binding variant");
    }
    if (const auto* una = dynamic_cast<const sema::Unary*>(&expr)) {
        return emitUnary(*una);
    }
    if (const auto* bin = dynamic_cast<const sema::BinOp*>(&expr)) {
        return emitBinOp(*bin);
    }
    if (const auto* call = dynamic_cast<const sema::Call*>(&expr)) {
        if (call->callee == "__cl_f16_printfn" || call->callee == "__cl_f32_printfn") {
            auto* void_ty = llvm::Type::getVoidTy(ctx_);
            auto* float_ty = llvm::Type::getFloatTy(ctx_);
            auto* fn_ty = llvm::FunctionType::get(void_ty, {float_ty}, false);

            const std::string FN_NAME = call->callee;
            llvm::Function* callee = mod_->getFunction(FN_NAME);
            if (callee != nullptr) {
                if (callee->getFunctionType() != fn_ty) {
                    if (!callee->use_empty()) {
                        callee->setName(FN_NAME + "_OLD");
                    } else {
                        callee->eraseFromParent();
                    }

                    callee = nullptr;
                }
            }

            if (callee == nullptr) {
                auto f_callee = mod_->getOrInsertFunction(FN_NAME, fn_ty);
                callee = llvm::cast<llvm::Function>(f_callee.getCallee());
                callee->setCallingConv(llvm::CallingConv::C);
            }

            if (call->args.size() != 1) {
                throw std::runtime_error(FN_NAME + " expected 1 args");
            }
            llvm::Value* arg0 = emitExpr(*call->args[0]);
            if (arg0->getType()->isHalfTy()) {
                arg0 = builder_->CreateFPExt(arg0, float_ty, "h2f");
            } else if (arg0->getType()->isFloatTy()) {
            } else {
                throw std::runtime_error(FN_NAME + ": doesn't support other then float type");
            }

            return builder_->CreateCall(callee, {arg0});
        }

        llvm::Function* callee = mod_->getFunction(call->callee);
        if (callee == nullptr) {
            std::vector<llvm::Type*> param_tys;
            param_tys.reserve(call->args.size());
            for (const auto& args : call->args) {
                param_tys.push_back(toLlvmType(args->type));
            }
            auto* ret_ty = toLlvmType(call->type);
            auto* fn_ty = llvm::FunctionType::get(ret_ty, param_tys, false);
            auto f_callee = mod_->getOrInsertFunction(call->callee, fn_ty);
            callee = llvm::cast<llvm::Function>(f_callee.getCallee());
        }
        std::vector<llvm::Value*> args;
        args.reserve(call->args.size());
        for (const auto& arg : call->args) {
            args.push_back(emitExpr(*arg));
        }
        return builder_->CreateCall(callee, args);
    }
    if (const auto* cast = dynamic_cast<const sema::Cast*>(&expr)) {
        llvm::Value* val = emitExpr(*cast->inner);
        const TypeRef& src = cast->inner->type;
        const TypeRef& dst = cast->target_type;

        if (!src.isBuiltin() || !dst.isBuiltin()) {
            throw std::runtime_error("emitCast: non-builtin types are not supported");
        }

        if (src.builtin.kind == dst.builtin.kind) {
            return val;
        }

        auto* dst_ty = toLlvmType(dst);

        auto is_int_kind = [](const Type::kind_enum KIND) {
            switch (KIND) {
            case Type::kind_enum::I8:
            case Type::kind_enum::U8:
            case Type::kind_enum::I16:
            case Type::kind_enum::U16:
            case Type::kind_enum::I32:
            case Type::kind_enum::U32:
            case Type::kind_enum::I64:
            case Type::kind_enum::U64:
                return true;
            case Type::kind_enum::STRING:
            case Type::kind_enum::NORETURN:
            case Type::kind_enum::UNIT:
            case Type::kind_enum::F16:
            case Type::kind_enum::F32:
                return false;
            }
            return false;
        };

        if (is_int_kind(src.builtin.kind) && dst.builtin.kind == Type::kind_enum::F16) {
            if (!val->getType()->isIntegerTy() || !dst_ty->isHalfTy()) {
                throw std::runtime_error("emitCast(int<->f16): type mismatch");
            }

            return src.builtin.isUnsigned() ? builder_->CreateUIToFP(val, dst_ty)
                                            : builder_->CreateSIToFP(val, dst_ty);
        }

        if (src.builtin.kind == Type::kind_enum::F16 && is_int_kind(dst.builtin.kind)) {
            if (!val->getType()->isHalfTy() || !dst_ty->isIntegerTy()) {
                throw std::runtime_error("emitCast(f16->int): type mismatch");
            }
            return dst.builtin.isUnsigned() ? builder_->CreateFPToUI(val, dst_ty)
                                            : builder_->CreateFPToSI(val, dst_ty);
        }

        if (is_int_kind(src.builtin.kind) && dst.builtin.kind == Type::kind_enum::F32) {
            if (!val->getType()->isIntegerTy() || !dst_ty->isFloatTy()) {
                throw std::runtime_error("emitCast (int<->f32): type mismatch");
            }

            return src.builtin.isUnsigned() ? builder_->CreateUIToFP(val, dst_ty)
                                            : builder_->CreateSIToFP(val, dst_ty);
        }

        if (src.builtin.kind == Type::kind_enum::F32 && is_int_kind(dst.builtin.kind)) {
            if (!val->getType()->isFloatTy() || !dst_ty->isIntegerTy()) {
                throw std::runtime_error("emitCast(f32->int): type mismatch");
            }
            return dst.builtin.isUnsigned() ? builder_->CreateFPToUI(val, dst_ty)
                                            : builder_->CreateFPToSI(val, dst_ty);
        }

        throw std::runtime_error("emitCast: unsupported cast");
    }
    throw std::runtime_error("emitExpr: unsupported node");
}

llvm::Value* IrGenFromSema::emitUnary(const sema::Unary& unary) {
    auto* val = emitExpr(*unary.inner);
    if (unary.op == "-") {
        if (val->getType()->isIntegerTy()) {
            return builder_->CreateNeg(val);
        }
        if (val->getType()->isHalfTy() || val->getType()->isFloatTy()) {
            llvm::Value* zero = llvm::ConstantFP::get(val->getType(), 0.0);
            return builder_->CreateFSub(zero, val);
        }
    }
    throw std::runtime_error("emitUnary: unsupported op/type");
}

llvm::Value* IrGenFromSema::emitBinOp(const sema::BinOp& bin) {
    auto* lhs = emitExpr(*bin.lhs);
    auto* rhs = emitExpr(*bin.rhs);

    if (bin.op == "is" || bin.op == "not") {
        llvm::Value* cmp = nullptr;
        if (lhs->getType()->isIntegerTy() && lhs->getType() == rhs->getType()) {
            cmp = (bin.op == "is") ? builder_->CreateICmpEQ(lhs, rhs)
                                   : builder_->CreateICmpNE(lhs, rhs);
        } else if ((lhs->getType()->isHalfTy() && rhs->getType()->isHalfTy()) ||
                   (lhs->getType()->isFloatTy() && rhs->getType()->isFloatTy())) {
            cmp = (bin.op == "is") ? builder_->CreateFCmpOEQ(lhs, rhs)
                                   : builder_->CreateFCmpONE(lhs, rhs);
        } else {
            throw std::runtime_error("emitBinOp: unsupported type for equal");
        }

        return cmp;
    }

    if (bin.op == "and" || bin.op == "or") {
        // Coerce operands to i1
        auto to_i1 = [&](llvm::Value* val) -> llvm::Value* {
            if (val->getType()->isIntegerTy(1)) {
                return val;
            }
            if (val->getType()->isIntegerTy()) {
                return builder_->CreateICmpNE(val, llvm::ConstantInt::get(val->getType(), 0));
            }
            if (val->getType()->isHalfTy() || val->getType()->isFloatTy()) {
                return builder_->CreateFCmpONE(val, llvm::ConstantFP::get(val->getType(), 0.0));
            }
            if (val->getType()->isPointerTy()) {
                return builder_->CreateICmpNE(
                    val,
                    llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(val->getType())));
            }
            throw std::runtime_error("emitBinOp(and|or): cannot coerce operand to i1");
        };
        llvm::Value* lhs_i1 = to_i1(lhs);
        llvm::Value* rhs_i1 = to_i1(rhs);
        return (bin.op == "and") ? builder_->CreateAnd(lhs_i1, rhs_i1)
                                 : builder_->CreateOr(lhs_i1, rhs_i1);
    }

    if (lhs->getType()->isIntegerTy() && lhs->getType() == rhs->getType()) {
        const bool IS_UNSIGNED =
            bin.lhs->type.builtin.isUnsigned() && bin.rhs->type.builtin.isUnsigned();

        if (bin.op == "+") {
            return builder_->CreateAdd(lhs, rhs);
        }
        if (bin.op == "-") {
            return builder_->CreateSub(lhs, rhs);
        }
        if (bin.op == "*") {
            return builder_->CreateMul(lhs, rhs);
        }
        if (bin.op == "/") {
            return IS_UNSIGNED ? builder_->CreateUDiv(lhs, rhs) : builder_->CreateSDiv(lhs, rhs);
        }
        if (bin.op == "%") {
            return IS_UNSIGNED ? builder_->CreateURem(lhs, rhs) : builder_->CreateSRem(lhs, rhs);
        }
    }
    if ((lhs->getType()->isHalfTy() && rhs->getType()->isHalfTy()) ||
        (lhs->getType()->isFloatTy() && rhs->getType()->isFloatTy())) {
        if (bin.op == "+") {
            return builder_->CreateFAdd(lhs, rhs);
        }
        if (bin.op == "-") {
            return builder_->CreateFSub(lhs, rhs);
        }
        if (bin.op == "*") {
            return builder_->CreateFMul(lhs, rhs);
        }
        if (bin.op == "/") {
            return builder_->CreateFDiv(lhs, rhs);
        }
    } else if (bin.lhs->type.isBuiltin() && bin.rhs->type.isBuiltin() &&
               bin.lhs->type.builtin.kind == Type::kind_enum::STRING &&
               bin.rhs->type.builtin.kind == Type::kind_enum::STRING) {
        if (bin.op != "+") {
            throw std::runtime_error("emitBinOp: unsupported string op (only +)");
        }

        auto* i8_ptr = llvm::Type::getInt8Ty(ctx_)->getPointerTo();
        auto* fnt_ty = llvm::FunctionType::get(i8_ptr, {i8_ptr, i8_ptr}, false);
        llvm::Function* func = mod_->getFunction("__cl_string_concat");
        if (func == nullptr) {
            func = llvm::Function::Create(fnt_ty, llvm::Function::ExternalLinkage,
                                          "__cl_string_concat", mod_.get());
        }
        return builder_->CreateCall(func, {lhs, rhs});
    }
    throw std::runtime_error("emitBinOp: unsupported op/type");
}

llvm::Type* IrGenFromSema::toLlvmType(const TypeRef& t_ref) const {
    if (!t_ref.isBuiltin()) {
        throw std::runtime_error("toLlvmType: non-builtin");
    }
    switch (t_ref.builtin.kind) {
    case Type::kind_enum::I8:
    case Type::kind_enum::U8:
        return llvm::Type::getInt8Ty(ctx_);
    case Type::kind_enum::I16:
    case Type::kind_enum::U16:
        return llvm::Type::getInt16Ty(ctx_);

    case Type::kind_enum::I32:
    case Type::kind_enum::U32:
        return llvm::Type::getInt32Ty(ctx_);
    case Type::kind_enum::I64:
    case Type::kind_enum::U64:
        return llvm::Type::getInt64Ty(ctx_);
    case Type::kind_enum::F16:
        return llvm::Type::getHalfTy(ctx_);
    case Type::kind_enum::F32:
        return llvm::Type::getFloatTy(ctx_);
    case Type::kind_enum::UNIT:
    case Type::kind_enum::NORETURN:
        return llvm::Type::getVoidTy(ctx_);
    case Type::kind_enum::STRING:
        return llvm::Type::getInt8Ty(ctx_)->getPointerTo();
    case Type::kind_enum::BOOLEAN:
        return llvm::Type::getInt1Ty(ctx_);
    }
    throw std::runtime_error("toLlvmType: unsupported kind");
}

llvm::Constant* IrGenFromSema::toLlvmConst(const Value& val) const {
    if (!val.type.isBuiltin()) {
        throw std::runtime_error("toLlvmConst: non-builtin");
    }
    switch (val.type.builtin.kind) {
    case Type::kind_enum::I8: {
        auto n_variant = asNum2(val);
        int64_t i_holds = std::holds_alternative<int64_t>(n_variant)
                              ? std::get<int64_t>(n_variant)
                              : static_cast<int64_t>(std::get<uint64_t>(n_variant));
        return llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx_), i_holds, true);
    }
    case Type::kind_enum::U8: {
        auto n_variant = asNum2(val);
        uint64_t u_holds = std::holds_alternative<uint64_t>(n_variant)
                               ? std::get<uint64_t>(n_variant)
                               : static_cast<uint64_t>(std::get<int64_t>(n_variant));
        return llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx_), u_holds, false);
    }
    case Type::kind_enum::I16: {
        auto* ty16 = llvm::Type::getInt16Ty(ctx_);
        auto n_variant = asNum2(val);
        int64_t i_holds = std::holds_alternative<int64_t>(n_variant)
                              ? std::get<int64_t>(n_variant)
                              : static_cast<int64_t>(std::get<uint64_t>(n_variant));
        return llvm::ConstantInt::get(ty16, i_holds, true);
    }
    case Type::kind_enum::U16: {
        auto* ty16 = llvm::Type::getInt16Ty(ctx_);
        auto n_variant = asNum2(val);
        uint64_t u_holds = std::holds_alternative<uint64_t>(n_variant)
                               ? std::get<uint64_t>(n_variant)
                               : static_cast<uint64_t>(std::get<int64_t>(n_variant));
        return llvm::ConstantInt::get(ty16, u_holds, false);
    }
    case Type::kind_enum::I32: {
        auto n_variant = asNum2(val);
        int64_t i_holds = std::holds_alternative<int64_t>(n_variant)
                              ? std::get<int64_t>(n_variant)
                              : static_cast<int64_t>(std::get<uint64_t>(n_variant));
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), i_holds, true);
    }
    case Type::kind_enum::U32: {
        auto n_variant = asNum2(val);
        uint64_t u_holds = std::holds_alternative<uint64_t>(n_variant)
                               ? std::get<uint64_t>(n_variant)
                               : static_cast<uint64_t>(std::get<int64_t>(n_variant));
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), u_holds, false);
    }
    case Type::kind_enum::I64: {
        auto n_variant = asNum2(val);
        int64_t i_holds = std::holds_alternative<int64_t>(n_variant)
                              ? std::get<int64_t>(n_variant)
                              : static_cast<int64_t>(std::get<uint64_t>(n_variant));
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx_), i_holds, true);
    }
    case Type::kind_enum::U64: {
        auto n_variant = asNum2(val);
        uint64_t u_holds = std::holds_alternative<uint64_t>(n_variant)
                               ? std::get<uint64_t>(n_variant)
                               : static_cast<uint64_t>(std::get<int64_t>(n_variant));
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx_), u_holds, false);
    }
    case Type::kind_enum::F16: {
        const ClHalf F16 = std::get<ClHalf>(val.v);
        llvm::APFloat apf(llvm::APFloat::IEEEhalf(), llvm::APInt(16, F16.bits));
        return llvm::ConstantFP::get(llvm::Type::getHalfTy(ctx_), apf);
    }
    case Type::kind_enum::F32: {
        const ClF32 F32 = std::get<ClF32>(val.v);
        llvm::APFloat apf(llvm::APFloat::IEEEsingle(), llvm::APInt(32, F32.bits));
        return llvm::ConstantFP::get(llvm::Type::getFloatTy(ctx_), apf);
    }
    case Type::kind_enum::BOOLEAN: {
        bool bool_holds = false;

        if (std::holds_alternative<int64_t>(val.v)) {
            bool_holds = std::get<int64_t>(val.v) != 0;
        } // unexpected uint64_t type

        return llvm::ConstantInt::get(llvm::Type::getInt1Ty(ctx_),
                                      static_cast<uint64_t>(bool_holds));
    }
    case Type::kind_enum::UNIT:
    case Type::kind_enum::NORETURN:
        throw std::runtime_error("toLlvmConst: unit/noreturn has no value");
    case Type::kind_enum::STRING:
        const auto& str = asString(val);
        auto* arr = llvm::ConstantDataArray::getString(ctx_, str, true);

        auto* g_var = new llvm::GlobalVariable(*mod_, arr->getType(),
                                               true, // isConstant
                                               llvm::GlobalValue::PrivateLinkage, arr, "");
        g_var->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        g_var->setAlignment(llvm::Align(1));

        llvm::Constant* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), 0);
        llvm::Constant* idxs[] = {zero, zero};
        return llvm::ConstantExpr::getInBoundsGetElementPtr(arr->getType(), g_var, idxs);
    }
    throw std::runtime_error("toLlvmConst: unsupported kind");
}
