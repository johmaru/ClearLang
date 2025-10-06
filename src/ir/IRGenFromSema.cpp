#include "IRGenFromSema.h"
#include <cstdint>
#include <llvm/IR/Constants.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/CFG.h>

ir_gen_from_sema::ir_gen_from_sema(llvm::LLVMContext& ctx, const std::string& module_name)
  : ctx_(ctx),
    mod_(std::make_unique<llvm::Module>(module_name, ctx_)),
    builder_(std::make_unique<llvm::IRBuilder<>>(ctx_)) {}

llvm::Module& ir_gen_from_sema::module() const { return *mod_; }

void ir_gen_from_sema::emit_module(const sema::module& m) {
    // Predeclare all functions with proper signatures
    for (auto& f : m.functions) {
        std::vector<llvm::Type*> param_tys;
        for (auto& p : f->params) param_tys.push_back(to_llvm_type(p.type));
        auto* ret_ty = to_llvm_type(f->return_type);
        auto* fn_ty = llvm::FunctionType::get(ret_ty, param_tys, false);
        (void)mod_->getOrInsertFunction(f->name, fn_ty);
    }
    // Then emit bodies
    for (auto& f : m.functions) emit_function(*f);
    emit_entry_shim(m);
    
    // Exit code
    if (!mod_->getFunction("__cl_exit_code")) {
        auto* i32_ty = llvm::Type::getInt32Ty(ctx_);
        auto* ft = llvm::FunctionType::get(i32_ty, {}, false);
        auto* fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "__cl_exit_code", mod_.get());
        auto* bb = llvm::BasicBlock::Create(ctx_, "entry", fn);
        builder_->SetInsertPoint(bb);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 0));
    }
}

std::unique_ptr<llvm::Module> ir_gen_from_sema::take_module() {
    return std::move(mod_);
}

void ir_gen_from_sema::emit_entry_shim(const sema::module& m) const {
    if (m.entry_name.empty()) return; // no entry point
    llvm::Function* entry = mod_->getFunction(m.entry_name);
    if (!entry) return;
    if (entry->arg_size() != 0) return;

    auto& c = ctx_;
    auto* i8_ty = llvm::Type::getInt8Ty(c);
    auto* i16_ty = llvm::Type::getInt16Ty(c);
    auto* i32_ty = llvm::Type::getInt32Ty(c);
    auto* i8_ptr_ty = llvm::Type::getInt8Ty(c)->getPointerTo();

    auto* ft = llvm::FunctionType::get(i32_ty, {i8_ptr_ty, i32_ty}, false);
    auto* fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "__cl_entry", mod_.get());
    auto* bb = llvm::BasicBlock::Create(c, "entry", fn);
    builder_->SetInsertPoint(bb);

    auto ai = fn->arg_begin();
    llvm::Value* out = ai++;

    auto store_tag = [&](const uint8_t tag) {
        builder_->CreateStore(llvm::ConstantInt::get(i8_ty, tag), out);
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
    if (ret_ty->isIntegerTy(8)){
	    llvm::Value* v = builder_->CreateCall(entry, {});
	    store_tag(2);
	    auto* p8 = builder_->CreateBitCast(data_ptr, i8_ty->getPointerTo());
	    builder_->CreateStore(v, p8);
	    builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 2)); // 1(tag) + 1 (data)
	    return;
    }
    if (ret_ty->isIntegerTy(16)){
        llvm::Value* v = builder_->CreateCall(entry, {});
        store_tag(3);
        auto* p16 = builder_->CreateBitCast(data_ptr, i16_ty->getPointerTo());
        builder_->CreateStore(v, p16);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 3)); // 1(tag) + 2 (data)
        return;
	}
    if (ret_ty->isIntegerTy(32)){
	    llvm::Value* v = builder_->CreateCall(entry, {});
	    store_tag(4);
	    auto* p32 = builder_->CreateBitCast(data_ptr, i32_ty->getPointerTo());
	    builder_->CreateStore(v, p32);
	    builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 5)); // 1(tag) + 4 (data)
	    return;
    }
    if (ret_ty->isIntegerTy(64)){
	    llvm::Value* v = builder_->CreateCall(entry, {});
	    store_tag(5);
	    auto* p64 = builder_->CreateBitCast(data_ptr, llvm::Type::getInt64Ty(c)->getPointerTo());
	    builder_->CreateStore(v, p64);
	    builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 9)); // 1(tag) + 8 (data)
	    return;
    }
    if (ret_ty->isHalfTy()) {
	    llvm::Value* h = builder_->CreateCall(entry, {});
	    llvm::Value* bits = builder_->CreateBitCast(h, i16_ty);
	    store_tag(6);
	    auto* p16 = builder_->CreateBitCast(data_ptr, i16_ty->getPointerTo());
	    builder_->CreateStore(bits, p16);
	    builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 3)); // 1(tag) + 2 (data)
	    return;
    }
    if (ret_ty->isFloatTy()) {
        llvm::Value* h = builder_->CreateCall(entry, {});
        llvm::Value* bits = builder_->CreateBitCast(h, i32_ty);
        store_tag(7);
        auto* p32 = builder_->CreateBitCast(data_ptr, i32_ty->getPointerTo());
        builder_->CreateStore(bits, p32);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 5));
        return;
    }

    if (ret_ty->isIntegerTy(1)) {
        llvm::Value* v = builder_->CreateCall(entry, {});

        store_tag(8);
        auto* p8 = builder_->CreateBitCast(data_ptr, i8_ty->getPointerTo());

        v = builder_->CreateZExt(v, i8_ty);
        builder_->CreateStore(v, p8);
        builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 2));
        return;
    }

    builder_->CreateRet(llvm::ConstantInt::get(i32_ty, 0));
}

llvm::Function* ir_gen_from_sema::emit_function(const sema::function& f) {
    std::vector<llvm::Type*> param_tys;
    param_tys.reserve(f.params.size());
    for (auto& p : f.params) param_tys.push_back(to_llvm_type(p.type));
    auto* ret_ty = to_llvm_type(f.return_type);
    auto* fn_ty = llvm::FunctionType::get(ret_ty, param_tys, false);

    // Reuse predeclared function if available
    llvm::Function* fn = mod_->getFunction(f.name);
    if (!fn) {
        auto callee = mod_->getOrInsertFunction(f.name, fn_ty);
        fn = llvm::cast<llvm::Function>(callee.getCallee());
    }

    unsigned idx = 0;
    for (auto& arg : fn->args()) {
        arg.setName(f.params[idx].name);
        ++idx;
    }

    auto* entry = llvm::BasicBlock::Create(ctx_, "entry", fn);
    builder_->SetInsertPoint(entry);

    vars_.emplace_back();
    idx = 0;
    for (auto& arg : fn->args()) {
        vars_.back()[std::string(arg.getName())] = &arg;
        ++idx;
    }
    emit_block(*f.body);

    // Verification currently InsertBlock.
    llvm::BasicBlock* cur = builder_->GetInsertBlock();
    if (!cur->getTerminator()) {
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
    return fn;
}

void ir_gen_from_sema::emit_block(const sema::block& b) {
    for (auto& s : b.statements) emit_stmt(*s);
}

void ir_gen_from_sema::emit_stmt(const sema::stmt& s) {
    if (auto* vd = dynamic_cast<const sema::stmt_var_decl*>(&s)) {
        auto* init = vd->init_expr ? emit_expr(*vd->init_expr) : nullptr;
        if (!init) {

		if (vd->decl_type.builtin.kind == type::kind_enum::string)
			init = llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(ctx_)->getPointerTo());
        else if (vd->decl_type.builtin.kind == type::kind_enum::f16 || vd->decl_type.builtin.kind == type::kind_enum::f32)
            init = llvm::ConstantFP::get(to_llvm_type(vd->decl_type), llvm::APFloat(0.0f));
        else if (vd->decl_type.builtin.kind == type::kind_enum::unit)
            init = nullptr; // no storage needed
        else if (vd->decl_type.builtin.kind == type::kind_enum::noreturn)
			throw std::runtime_error("variable cannot have noreturn type");

        else if (vd->decl_type.builtin.is_unsigned())
                init = llvm::ConstantInt::get(to_llvm_type(vd->decl_type), 0, false);
            else
                init = llvm::ConstantInt::get(to_llvm_type(vd->decl_type), 0, true);
        }
        vars_.back()[vd->name] = init;
    } else if (auto* ret = dynamic_cast<const sema::stmt_return*>(&s)) {
        if (!ret->value) {
            builder_->CreateRetVoid();
        } else {
            auto* v = emit_expr(*ret->value);
            builder_->CreateRet(v);
        }
    } else if (auto* sif = dynamic_cast<const sema::stmt_if*>(&s)) {
        llvm::Value* cond_v = emit_expr(*sif->cond);
        llvm::Value* cond_i1;

        if (cond_v->getType()->isIntegerTy(1)) {
            cond_i1 = cond_v;
        } else if (cond_v->getType()->isIntegerTy()) {
            cond_i1 = builder_->CreateICmpNE(cond_v, llvm::ConstantInt::get(cond_v->getType(), 0));
        } else if (cond_v->getType()->isHalfTy() || cond_v->getType()->isFloatTy()) {
            cond_i1 = builder_->CreateFCmpONE(cond_v, llvm::ConstantFP::get(cond_v->getType(), 0.0));
        } else if (cond_v->getType()->isPointerTy()) {
            cond_i1 = builder_->CreateICmpNE(cond_v, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cond_v->getType())));
        } else {
            throw std::runtime_error("if condition: unsupported type");
        }

        llvm::Function* fn = builder_->GetInsertBlock()->getParent();
        auto* then_blk = llvm::BasicBlock::Create(ctx_, "if.then", fn);
        llvm::BasicBlock* else_blk = nullptr;
        auto* cont_blk = llvm::BasicBlock::Create(ctx_, "if.end", fn);

        if (sif->else_blk) {
            else_blk = llvm::BasicBlock::Create(ctx_, "if.else", fn);
            builder_->CreateCondBr(cond_i1, then_blk, else_blk);
        } else {
            builder_->CreateCondBr(cond_i1, then_blk, cont_blk);
        }

        builder_->SetInsertPoint(then_blk);
        emit_block(*sif->then_blk);
        if (!builder_->GetInsertBlock()->getTerminator()) builder_->CreateBr(cont_blk);

        if (else_blk) {
            builder_->SetInsertPoint(else_blk);
            emit_block(*sif->else_blk);
            if (!builder_->GetInsertBlock()->getTerminator()) builder_->CreateBr(cont_blk);
        }

        builder_->SetInsertPoint(cont_blk);

    } else if (auto* se = dynamic_cast<const sema::stmt_expr*>(&s)) {
        (void)emit_expr(*se->expr);
    } else {
	    
    }
}

llvm::Value* ir_gen_from_sema::emit_expr(const sema::expr& e) {
    if (auto* lit = dynamic_cast<const sema::literal*>(&e)) {
        return to_llvm_const(lit->value);
    }
    if (auto* vr = dynamic_cast<const sema::var_ref*>(&e)) {
	    const auto it = vars_.back().find(vr->name);
	    if (it == vars_.back().end()) throw std::runtime_error("undefined var in IRGen: " + vr->name);
	    return it->second;
    }
    if (auto* u = dynamic_cast<const sema::unary*>(&e)) {
	    return emit_unary(*u);
    }
    if (auto* b = dynamic_cast<const sema::bin_op*>(&e)) {
	    return emit_bin_op(*b);
    }
    if (auto* c = dynamic_cast<const sema::call*>(&e)) {

        if (c->callee == "__cl_f16_printfn" || c->callee == "__cl_f32_printfn") {
            auto* void_ty = llvm::Type::getVoidTy(ctx_);
            auto* float_ty = llvm::Type::getFloatTy(ctx_);
            auto* fn_ty = llvm::FunctionType::get(void_ty, { float_ty }, false);

            const std::string fn_name = c->callee;
            llvm::Function* callee = mod_->getFunction(fn_name);
            if (callee) {
	            if (callee->getFunctionType() != fn_ty) {
		            if (!callee->use_empty()) callee->setName(fn_name + "_OLD");
		            else callee->eraseFromParent();
		            
                    callee = nullptr;
	            }
            }

            if (!callee) {
                auto fc = mod_->getOrInsertFunction(fn_name, fn_ty);
                callee = llvm::cast<llvm::Function>(fc.getCallee());
                callee->setCallingConv(llvm::CallingConv::C);
            }

            if (c->args.size() != 1) throw std::runtime_error(fn_name +" expected 1 args");
            llvm::Value* arg0 = emit_expr(*c->args[0]);
            if (arg0->getType()->isHalfTy()) {
	            arg0 = builder_->CreateFPExt(arg0, float_ty, "h2f");
            } else if (arg0->getType()->isFloatTy()) {}
              else throw std::runtime_error(fn_name +": doesn't support other then float type");

            return builder_->CreateCall(callee, { arg0 });
        }

	    llvm::Function* callee = mod_->getFunction(c->callee);
	    if (!callee) {
		    std::vector<llvm::Type*> param_tys;
		    param_tys.reserve(c->args.size());
		    for (auto& a : c->args) param_tys.push_back(to_llvm_type(a->type));
		    auto* ret_ty = to_llvm_type(c->type);
		    auto* fn_ty = llvm::FunctionType::get(ret_ty, param_tys, false);
		    auto fi = mod_->getOrInsertFunction(c->callee, fn_ty);
		    callee = llvm::cast<llvm::Function>(fi.getCallee());
	    }
	    std::vector<llvm::Value*> args;
	    args.reserve(c->args.size());
	    for (auto& a : c->args) args.push_back(emit_expr(*a));
	    return builder_->CreateCall(callee, args);
    }
    if (auto* cast = dynamic_cast<const sema::cast*>(&e)) {
        llvm::Value* v = emit_expr(*cast->inner);
        const type_ref& src = cast->inner->type;
        const type_ref& dst = cast->target_type;

        if (!src.is_builtin() || !dst.is_builtin())
            throw std::runtime_error("emitCast: non-builtin types are not supported");

        if (src.builtin.kind == dst.builtin.kind) return v;

        auto* dst_ty = to_llvm_type(dst);

        auto is_int_kind = [](const type::kind_enum k) {
            switch (k) {
            case type::kind_enum::i8: case type::kind_enum::u8:
            case type::kind_enum::i16: case type::kind_enum::u16:
            case type::kind_enum::i32: case type::kind_enum::u32:
            case type::kind_enum::i64: case type::kind_enum::u64:
                return true;
            case type::kind_enum::string: case type::kind_enum::noreturn: case type::kind_enum::unit: case type::kind_enum::f16: case type::kind_enum::f32:
                return false;
            }
            return false;
        };

        if (is_int_kind(src.builtin.kind) && dst.builtin.kind == type::kind_enum::f16) {
            if (!v->getType()->isIntegerTy() || !dst_ty->isHalfTy())
                throw std::runtime_error("emitCast(int<->f16): type mismatch");

            return src.builtin.is_unsigned()
                ? builder_->CreateUIToFP(v, dst_ty)
                : builder_->CreateSIToFP(v, dst_ty);
        }

        if (src.builtin.kind == type::kind_enum::f16 && is_int_kind(dst.builtin.kind)) {
            if (!v->getType()->isHalfTy() || !dst_ty->isIntegerTy())
                throw std::runtime_error("emitCast(f16->int): type mismatch");
            return dst.builtin.is_unsigned()
                ? builder_->CreateFPToUI(v, dst_ty)
                : builder_->CreateFPToSI(v, dst_ty);
        }

        if (is_int_kind(src.builtin.kind) && dst.builtin.kind == type::kind_enum::f32) {
            if (!v->getType()->isIntegerTy() || !dst_ty->isFloatTy())
                throw std::runtime_error("emitCast (int<->f16): type mismatch");

            return src.builtin.is_unsigned()
                ? builder_->CreateUIToFP(v, dst_ty)
				: builder_->CreateSIToFP(v, dst_ty);
        }

        if (src.builtin.kind == type::kind_enum::f32 && is_int_kind(dst.builtin.kind)) {
            if (!v->getType()->isFloatTy() || !dst_ty->isIntegerTy())
                throw std::runtime_error("emitCast(f32->int): type mismatch");
            return dst.builtin.is_unsigned()
                ? builder_->CreateFPToUI(v, dst_ty)
                : builder_->CreateFPToSI(v, dst_ty);
        }

        throw std::runtime_error("emitCast: unsupported cast");
    }
    throw std::runtime_error("emitExpr: unsupported node");
}

llvm::Value* ir_gen_from_sema::emit_unary(const sema::unary& u) {
    auto* v = emit_expr(*u.inner);
    if (u.op == "-") {
        if (v->getType()->isIntegerTy()) {
            return builder_->CreateNeg(v);
        }
        if (v->getType()->isHalfTy() || v->getType()->isFloatTy()) {
	        llvm::Value* zero = llvm::ConstantFP::get(v->getType(), 0.0);
	        return builder_->CreateFSub(zero, v);
        }
    }
    throw std::runtime_error("emitUnary: unsupported op/type");
}

llvm::Value* ir_gen_from_sema::emit_bin_op(const sema::bin_op& b) {
    auto* lhs = emit_expr(*b.lhs);
    auto* rhs = emit_expr(*b.rhs);

    if (b.op == "is" || b.op == "not") {
        llvm::Value* cmp = nullptr;
        if (lhs->getType()->isIntegerTy() && lhs->getType() == rhs->getType()) {
            cmp = (b.op == "is") ? builder_->CreateICmpEQ(lhs, rhs)
								 : builder_->CreateICmpNE(lhs, rhs);
        } else if ((lhs->getType()->isHalfTy() && rhs->getType()->isHalfTy()) || (lhs->getType()->isFloatTy() && rhs->getType()->isFloatTy())) {
            cmp = (b.op == "is") ? builder_->CreateFCmpOEQ(lhs, rhs)
                                 : builder_->CreateFCmpONE(lhs, rhs);
        } else throw std::runtime_error("emitBinOp: unsupported type for equal");

        return cmp;
    }

    if (b.op == "and" || b.op == ("or")) {
        // Coerce operands to i1
        auto to_i1 = [&](llvm::Value* v) -> llvm::Value* {
            if (v->getType()->isIntegerTy(1)) {
                return v;
            } else if (v->getType()->isIntegerTy()) {
                return builder_->CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0));
            } else if (v->getType()->isHalfTy() || v->getType()->isFloatTy()) {
                return builder_->CreateFCmpONE(v, llvm::ConstantFP::get(v->getType(), 0.0));
            } else if (v->getType()->isPointerTy()) {
                return builder_->CreateICmpNE(v, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(v->getType())));
            } else {
                throw std::runtime_error("emitBinOp(and|or): cannot coerce operand to i1");
            }
        };
        llvm::Value* lhs_i1 = to_i1(lhs);
        llvm::Value* rhs_i1 = to_i1(rhs);
        return (b.op == "and") ? builder_->CreateAnd(lhs_i1, rhs_i1)
    	                       : builder_->CreateOr(lhs_i1, rhs_i1);
    }

    if (lhs->getType()->isIntegerTy() && lhs->getType() == rhs->getType()) {

		const bool is_unsigned = b.lhs->type.builtin.is_unsigned() && b.rhs->type.builtin.is_unsigned();

        if (b.op == "+") return builder_->CreateAdd(lhs, rhs);
        if (b.op == "-") return builder_->CreateSub(lhs, rhs);
        if (b.op == "*") return builder_->CreateMul(lhs, rhs);
        if (b.op == "/") return is_unsigned
            ? builder_->CreateUDiv(lhs, rhs)
            : builder_->CreateSDiv(lhs, rhs);
        if (b.op == "%") return is_unsigned
    		? builder_->CreateURem(lhs, rhs)
    		: builder_->CreateSRem(lhs, rhs);

    } else if (lhs->getType()->isHalfTy() && rhs->getType()->isHalfTy() || lhs->getType()->isFloatTy() && rhs->getType()->isFloatTy()) {
        if (b.op == "+") return builder_->CreateFAdd(lhs, rhs);
        if (b.op == "-") return builder_->CreateFSub(lhs, rhs);
        if (b.op == "*") return builder_->CreateFMul(lhs, rhs);
        if (b.op == "/") return builder_->CreateFDiv(lhs, rhs);
    } else if (b.lhs->type.is_builtin() && b.rhs->type.is_builtin() && b.lhs->type.builtin.kind == type::kind_enum::string && b.rhs->type.builtin.kind == type::kind_enum::string) {
    	if (b.op != "+") {
            throw std::runtime_error("emitBinOp: unsupported string op (only +)");
        }

		auto* i8_ptr = llvm::Type::getInt8Ty(ctx_)->getPointerTo();
		auto* fnt_ty = llvm::FunctionType::get(i8_ptr, { i8_ptr, i8_ptr }, false);
		llvm::Function* f = mod_->getFunction("__cl_string_concat");
        if (!f) {
			f = llvm::Function::Create(fnt_ty, llvm::Function::ExternalLinkage, "__cl_string_concat", mod_.get());
        }
		return builder_->CreateCall(f, { lhs, rhs });
    }
    throw std::runtime_error("emitBinOp: unsupported op/type");
}

llvm::Type* ir_gen_from_sema::to_llvm_type(const type_ref& t) const {
    if (!t.is_builtin()) throw std::runtime_error("toLlvmType: non-builtin");
    switch (t.builtin.kind) {
        case type::kind_enum::i8:  
        case type::kind_enum::u8:
            return llvm::Type::getInt8Ty(ctx_);
		case type::kind_enum::i16:
		case type::kind_enum::u16:
            return llvm::Type::getInt16Ty(ctx_);

        case type::kind_enum::i32:
        case type::kind_enum::u32:
    		return llvm::Type::getInt32Ty(ctx_);
        case type::kind_enum::i64:
        case type::kind_enum::u64:
    		return llvm::Type::getInt64Ty(ctx_);
        case type::kind_enum::f16: return llvm::Type::getHalfTy(ctx_);
        case type::kind_enum::f32: return llvm::Type::getFloatTy(ctx_);
        case type::kind_enum::unit:
        case type::kind_enum::noreturn:
    		return llvm::Type::getVoidTy(ctx_);
		case type::kind_enum::string:
			return llvm::Type::getInt8Ty(ctx_)->getPointerTo();
		case type::kind_enum::boolean:
			return  llvm::Type::getInt1Ty(ctx_);
    }
	throw std::runtime_error("toLlvmType: unsupported kind");
}

llvm::Constant* ir_gen_from_sema::to_llvm_const(const value& v) const {
    if (!v.type.is_builtin()) throw std::runtime_error("toLlvmConst: non-builtin");
    switch (v.type.builtin.kind) {
        case type::kind_enum::i8: {
            auto nv = as_num2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx_), s, true);
        }
        case type::kind_enum::u8: {
            auto nv = as_num2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx_), u, false);
        }
        case type::kind_enum::i16: {
            auto* ty16 = llvm::Type::getInt16Ty(ctx_);
            auto nv = as_num2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(ty16, s, true);
        }
        case type::kind_enum::u16: {
            auto* ty16 = llvm::Type::getInt16Ty(ctx_);
            auto nv = as_num2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(ty16, u, false);
        }
        case type::kind_enum::i32: {
            auto nv = as_num2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), s, true);
        }
        case type::kind_enum::u32: {
            auto nv = as_num2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), u, false);
        }
        case type::kind_enum::i64: {
            auto nv = as_num2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx_), s, true);
        }
        case type::kind_enum::u64: {
            auto nv = as_num2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx_), u, false);
        }
        case type::kind_enum::f16: {
            const cl_half h = std::get<cl_half>(v.v);
            llvm::APFloat apf(llvm::APFloat::IEEEhalf(), llvm::APInt(16, h.bits));
            return llvm::ConstantFP::get(llvm::Type::getHalfTy(ctx_), apf);
        }
	    case type::kind_enum::f32: {
	        const cl_f32 h = std::get<cl_f32>(v.v);
	        llvm::APFloat apf(llvm::APFloat::IEEEsingle(), llvm::APInt(32, h.bits));
	        return llvm::ConstantFP::get(llvm::Type::getFloatTy(ctx_), apf);
	    }
		case type::kind_enum::boolean: {
            bool b = false;


            if (std::holds_alternative<int64_t>(v.v)) {
                b = std::get<int64_t>(v.v) != 0;
            } // unexpected uint64_t type

            return llvm::ConstantInt::get(llvm::Type::getInt1Ty(ctx_), b);
		}
        case type::kind_enum::unit:
		case type::kind_enum::noreturn:
			throw std::runtime_error("toLlvmConst: unit/noreturn has no value");
		case type::kind_enum::string:
            const auto& str = as_string(v);
            auto* arr = llvm::ConstantDataArray::getString(ctx_, str, true);

			auto* gv = new llvm::GlobalVariable(
				*mod_,
				arr->getType(),
				true, // isConstant
				llvm::GlobalValue::PrivateLinkage,
				arr,
				"");
            gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
            gv->setAlignment(llvm::Align(1));

			llvm::Constant* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), 0);
			llvm::Constant* idxs[] = { zero, zero };
            return llvm::ConstantExpr::getInBoundsGetElementPtr(arr->getType(), gv, idxs);
			
    }
	throw std::runtime_error("toLlvmConst: unsupported kind");
}
