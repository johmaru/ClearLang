#include "IRGenFromSema.h"
#include <cstdint>
#include <llvm/IR/Constants.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/GlobalVariable.h>

IRGenFromSema::IRGenFromSema(llvm::LLVMContext& ctx, std::string moduleName)
  : ctx_(ctx),
    mod_(std::make_unique<llvm::Module>(std::move(moduleName), ctx_)),
    builder_(std::make_unique<llvm::IRBuilder<>>(ctx_)) {}

llvm::Module& IRGenFromSema::module() { return *mod_; }

void IRGenFromSema::emitModule(const sema::Module& m) {
    // Predeclare all functions with proper signatures
    for (auto& f : m.functions) {
        std::vector<llvm::Type*> paramTys;
        for (auto& p : f->params) paramTys.push_back(toLlvmType(p.type));
        auto* retTy = toLlvmType(f->returnType);
        auto* fnTy = llvm::FunctionType::get(retTy, paramTys, false);
        (void)mod_->getOrInsertFunction(f->name, fnTy);
    }
    // Then emit bodies
    for (auto& f : m.functions) emitFunction(*f);
    emitEntryShim(m);
    
    // Exit code
    if (!mod_->getFunction("__cl_exit_code")) {
        auto* i32Ty = llvm::Type::getInt32Ty(ctx_);
        auto* ft = llvm::FunctionType::get(i32Ty, {}, false);
        auto* fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "__cl_exit_code", mod_.get());
        auto* bb = llvm::BasicBlock::Create(ctx_, "entry", fn);
        builder_->SetInsertPoint(bb);
        builder_->CreateRet(llvm::ConstantInt::get(i32Ty, 0));
    }
}

std::unique_ptr<llvm::Module> IRGenFromSema::takeModule() {
    return std::move(mod_);
}

void IRGenFromSema::emitEntryShim(const sema::Module& m) {
    if (m.entryName.empty()) return; // no entry point
    llvm::Function* entry = mod_->getFunction(m.entryName);
    if (!entry) return;
    if (entry->arg_size() != 0) return;

    auto& C = ctx_;
    auto* i8Ty = llvm::Type::getInt8Ty(C);
    auto* i16Ty = llvm::Type::getInt16Ty(C);
    auto* i32Ty = llvm::Type::getInt32Ty(C);
    auto* i8PtrTy = llvm::Type::getInt8Ty(C)->getPointerTo();

    auto* ft = llvm::FunctionType::get(i32Ty, {i8PtrTy, i32Ty}, false);
    auto* fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "__cl_entry", mod_.get());
    auto* bb = llvm::BasicBlock::Create(C, "entry", fn);
    builder_->SetInsertPoint(bb);

    auto ai = fn->arg_begin();
    llvm::Value* out = ai++;
    llvm::Value* cap = ai; (void)cap;

    auto storeTag = [&](uint8_t tag) {
        builder_->CreateStore(llvm::ConstantInt::get(i8Ty, tag), out);
    };

    auto* dataPtr = builder_->CreateGEP(i8Ty, out, llvm::ConstantInt::get(i32Ty, 1));

    llvm::Type* retTy = entry->getReturnType();
    if (retTy->isVoidTy()) {
        // unit -> tag=1, len=1
        storeTag(1);
        builder_->CreateRet(llvm::ConstantInt::get(i32Ty, 1));
        return;
    }
    if (retTy->isIntegerTy(8)){
	    llvm::Value* v = builder_->CreateCall(entry, {});
	    storeTag(2);
	    auto* p8 = builder_->CreateBitCast(dataPtr, i8Ty->getPointerTo());
	    builder_->CreateStore(v, p8);
	    builder_->CreateRet(llvm::ConstantInt::get(i32Ty, 2)); // 1(tag) + 1 (data)
	    return;
    }
    if (retTy->isIntegerTy(16)){
        llvm::Value* v = builder_->CreateCall(entry, {});
        storeTag(3);
        auto* p16 = builder_->CreateBitCast(dataPtr, i16Ty->getPointerTo());
        builder_->CreateStore(v, p16);
        builder_->CreateRet(llvm::ConstantInt::get(i32Ty, 3)); // 1(tag) + 2 (data)
        return;
	}
    if (retTy->isIntegerTy(32)){
	    llvm::Value* v = builder_->CreateCall(entry, {});
	    storeTag(4);
	    auto* p32 = builder_->CreateBitCast(dataPtr, i32Ty->getPointerTo());
	    builder_->CreateStore(v, p32);
	    builder_->CreateRet(llvm::ConstantInt::get(i32Ty, 5)); // 1(tag) + 4 (data)
	    return;
    }
    if (retTy->isIntegerTy(64)){
	    llvm::Value* v = builder_->CreateCall(entry, {});
	    storeTag(5);
	    auto* p64 = builder_->CreateBitCast(dataPtr, llvm::Type::getInt64Ty(C)->getPointerTo());
	    builder_->CreateStore(v, p64);
	    builder_->CreateRet(llvm::ConstantInt::get(i32Ty, 9)); // 1(tag) + 8 (data)
	    return;
    }
    if (retTy->isHalfTy()) {
	    llvm::Value* h = builder_->CreateCall(entry, {});
	    llvm::Value* bits = builder_->CreateBitCast(h, i16Ty);
	    storeTag(6);
	    auto* p16 = builder_->CreateBitCast(dataPtr, i16Ty->getPointerTo());
	    builder_->CreateStore(bits, p16);
	    builder_->CreateRet(llvm::ConstantInt::get(i32Ty, 3)); // 1(tag) + 2 (data)
	    return;
    }
    builder_->CreateRet(llvm::ConstantInt::get(i32Ty, 0));
}

llvm::Function* IRGenFromSema::emitFunction(const sema::Function& f) {
    std::vector<llvm::Type*> paramTys;
    for (auto& p : f.params) paramTys.push_back(toLlvmType(p.type));
    auto* retTy = toLlvmType(f.returnType);
    auto* fnTy = llvm::FunctionType::get(retTy, paramTys, false);

    // Reuse predeclared function if available
    llvm::Function* fn = mod_->getFunction(f.name);
    if (!fn) {
        auto callee = mod_->getOrInsertFunction(f.name, fnTy);
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
    emitBlock(*f.body);

    // if the last block does not have terminator, return void or error
    if (!entry->getTerminator()) {
        if (retTy->isVoidTy()) builder_->CreateRetVoid();
        else throw std::runtime_error("missing return");
    }
    vars_.pop_back();
    return fn;
}

void IRGenFromSema::emitBlock(const sema::Block& b) {
    for (auto& s : b.statements) emitStmt(*s);
}

void IRGenFromSema::emitStmt(const sema::Stmt& s) {
    if (auto* vd = dynamic_cast<const sema::StmtVarDecl*>(&s)) {
        auto* init = vd->initExpr ? emitExpr(*vd->initExpr) : nullptr;
        if (!init) {

		if (vd->declType.builtin.Kind == Type::STRING) {
			init = llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(ctx_)->getPointerTo());
        } else if (vd->declType.builtin.Kind == Type::F16) {
            init = llvm::ConstantFP::get(toLlvmType(vd->declType), llvm::APFloat(0.0f));
        } else if (vd->declType.builtin.Kind == Type::UNIT) {
            init = nullptr; // no storage needed
        } else if (vd->declType.builtin.Kind == Type::NORETURN) {
			throw std::runtime_error("variable cannot have noreturn type");
		}

        else if (vd->declType.builtin.isUnsigned())
                init = llvm::ConstantInt::get(toLlvmType(vd->declType), 0, false);
            else
                init = llvm::ConstantInt::get(toLlvmType(vd->declType), 0, true);
        }
        vars_.back()[vd->name] = init;
    } else if (auto* ret = dynamic_cast<const sema::StmtReturn*>(&s)) {
        if (!ret->value) {
            builder_->CreateRetVoid();
        } else {
            auto* v = emitExpr(*ret->value);
            builder_->CreateRet(v);
        }
    } else if (auto* se = dynamic_cast<const sema::StmtExpr*>(&s)) {
        (void)emitExpr(*se->expr);
    } else {
	    
    }
}

llvm::Value* IRGenFromSema::emitExpr(const sema::Expr& e) {
    if (auto* lit = dynamic_cast<const sema::Literal*>(&e)) {
        return toLlvmConst(lit->value);
    }
    if (auto* vr = dynamic_cast<const sema::VarRef*>(&e)) {
	    auto it = vars_.back().find(vr->name);
	    if (it == vars_.back().end()) throw std::runtime_error("undefined var in IRGen: " + vr->name);
	    return it->second;
    }
    if (auto* u = dynamic_cast<const sema::Unary*>(&e)) {
	    return emitUnary(*u);
    }
    if (auto* b = dynamic_cast<const sema::BinOp*>(&e)) {
	    return emitBinOp(*b);
    }
    if (auto* c = dynamic_cast<const sema::Call*>(&e)) {
	    llvm::Function* callee = mod_->getFunction(c->callee);
	    if (!callee) {
		    std::vector<llvm::Type*> paramTys;
		    for (auto& a : c->args) paramTys.push_back(toLlvmType(a->type));
		    auto* retTy = toLlvmType(c->type);
		    auto* fnTy = llvm::FunctionType::get(retTy, paramTys, false);
		    auto fi = mod_->getOrInsertFunction(c->callee, fnTy);
		    callee = llvm::cast<llvm::Function>(fi.getCallee());
	    }
	    std::vector<llvm::Value*> args;
	    args.reserve(c->args.size());
	    for (auto& a : c->args) args.push_back(emitExpr(*a));
	    return builder_->CreateCall(callee, args);
    }
    if (auto* cast = dynamic_cast<const sema::Cast*>(&e)) {
        llvm::Value* v = emitExpr(*cast->inner);
        const TypeRef& src = cast->inner->type;
        const TypeRef& dst = cast->targetType;

        if (!src.isBuiltin() || !dst.isBuiltin())
            throw std::runtime_error("emitCast: non-builtin types are not supported");

        if (src.builtin.Kind == dst.builtin.Kind) return v;

        auto* dstTy = toLlvmType(dst);

        auto is_int_kind = [](Type::KindEnum k) {
            switch (k) {
            case Type::I8: case Type::U8:
            case Type::I16: case Type::U16:
            case Type::I32: case Type::U32:
            case Type::I64: case Type::U64:
                return true;
            case Type::STRING: case Type::NORETURN: case Type::UNIT: case Type::F16:
                return false;
            }
            return false;
        };

        if (is_int_kind(src.builtin.Kind) && is_int_kind(dst.builtin.Kind)) {
            if (!v->getType()->isIntegerTy() || !dstTy->isIntegerTy())
                throw std::runtime_error("emitCast(int<->int): llvm types not integer");

            auto* srcITy = llvm::cast<llvm::IntegerType>(v->getType());
            auto* dstITy = llvm::cast<llvm::IntegerType>(dstTy);
            unsigned sbw = srcITy->getBitWidth();
            unsigned dbw = dstITy->getBitWidth();

            if (sbw == dbw) {
                return v;
            }
            if (sbw < dbw) {
	            return src.builtin.isUnsigned()
		                   ? builder_->CreateZExt(v, dstITy)
		                   : builder_->CreateSExt(v, dstITy);
            }
            return builder_->CreateTrunc(v, dstITy);
        }

        if (src.builtin.Kind == Type::F16 && is_int_kind(dst.builtin.Kind)) {
            if (!v->getType()->isHalfTy() || !dstTy->isIntegerTy())
                throw std::runtime_error("emitCast(f16->int): type mismatch");
            return dst.builtin.isUnsigned()
                ? builder_->CreateFPToUI(v, dstTy)
                : builder_->CreateFPToSI(v, dstTy);
        }

        throw std::runtime_error("emitCast: unsupported cast");
    }
    throw std::runtime_error("emitExpr: unsupported node");
}

llvm::Value* IRGenFromSema::emitUnary(const sema::Unary& u) {
    auto* v = emitExpr(*u.inner);
    if (u.op == "-") {
        if (v->getType()->isIntegerTy()) {
            return builder_->CreateNeg(v);
        } else if (v->getType()->isHalfTy()) {
            llvm::Value* zero = llvm::ConstantFP::get(v->getType(), 0.0);
            return builder_->CreateFSub(zero, v);
        }
    }
    throw std::runtime_error("emitUnary: unsupported op/type");
}

llvm::Value* IRGenFromSema::emitBinOp(const sema::BinOp& b) {
    auto* lhs = emitExpr(*b.lhs);
    auto* rhs = emitExpr(*b.rhs);
    if (lhs->getType()->isIntegerTy() && lhs->getType() == rhs->getType()) {

		const bool isUnsigned = b.lhs->type.builtin.isUnsigned() && b.rhs->type.builtin.isUnsigned();

        if (b.op == "+") return builder_->CreateAdd(lhs, rhs);
        if (b.op == "-") return builder_->CreateSub(lhs, rhs);
        if (b.op == "*") return builder_->CreateMul(lhs, rhs);
        if (b.op == "/") return isUnsigned
            ? builder_->CreateUDiv(lhs, rhs)
            : builder_->CreateSDiv(lhs, rhs);
		// TODO: mod, bitwise ops, shifts
        // if (b.op == "%") return isUnsigned ? builder_->CreateURem(lhs, rhs)
        //                                    : builder_->CreateSRem(lhs, rhs);
    } else if (lhs->getType()->isHalfTy() && rhs->getType()->isHalfTy()) {
        if (b.op == "+") return builder_->CreateFAdd(lhs, rhs);
        if (b.op == "-") return builder_->CreateFSub(lhs, rhs);
        if (b.op == "*") return builder_->CreateFMul(lhs, rhs);
        if (b.op == "/") return builder_->CreateFDiv(lhs, rhs);
    } else if (b.lhs->type.isBuiltin() && b.rhs->type.isBuiltin()
         && b.lhs->type.builtin.Kind == Type::STRING
         && b.rhs->type.builtin.Kind == Type::STRING) {

    	if (b.op != "+") {
            throw std::runtime_error("emitBinOp: unsupported string op (only +)");
        }

		auto* i8Ptr = llvm::Type::getInt8Ty(ctx_)->getPointerTo();
		auto* fntTy = llvm::FunctionType::get(i8Ptr, { i8Ptr, i8Ptr }, false);
		llvm::Function* f = mod_->getFunction("__cl_string_concat");
        if (!f) {
			f = llvm::Function::Create(fntTy, llvm::Function::ExternalLinkage, "__cl_string_concat", mod_.get());
        }
		return builder_->CreateCall(f, { lhs, rhs });
    }
    throw std::runtime_error("emitBinOp: unsupported op/type");
}

llvm::Type* IRGenFromSema::toLlvmType(const TypeRef& t) {
    if (!t.isBuiltin()) throw std::runtime_error("toLlvmType: non-builtin");
    switch (t.builtin.Kind) {
        case Type::I8:  
        case Type::U8:
            return llvm::Type::getInt8Ty(ctx_);
		case Type::I16:
		case Type::U16:
            return llvm::Type::getInt16Ty(ctx_);

        case Type::I32:
        case Type::U32:
    		return llvm::Type::getInt32Ty(ctx_);
        case Type::I64:
        case Type::U64:
    		return llvm::Type::getInt64Ty(ctx_);
        case Type::F16: return llvm::Type::getHalfTy(ctx_);
        case Type::UNIT:
        case Type::NORETURN:
    		return llvm::Type::getVoidTy(ctx_);
    case Type::STRING:
		return llvm::Type::getInt8Ty(ctx_)->getPointerTo();
    }
	throw std::runtime_error("toLlvmType: unsupported kind");
}

llvm::Constant* IRGenFromSema::toLlvmConst(const Value& v) {
    if (!v.type.isBuiltin()) throw std::runtime_error("toLlvmConst: non-builtin");
    switch (v.type.builtin.Kind) {
        case Type::I8: {
            auto nv = asNum2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : (int64_t)std::get<uint64_t>(nv);
            return llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx_), s, true);
        }
        case Type::U8: {
            auto nv = asNum2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : (uint64_t)std::get<int64_t>(nv);
            return llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx_), u, false);
        }
        case Type::I16: {
            auto* ty16 = llvm::Type::getInt16Ty(ctx_);
            auto nv = asNum2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(ty16, s, true);
        }
        case Type::U16: {
            auto* ty16 = llvm::Type::getInt16Ty(ctx_);
            auto nv = asNum2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(ty16, u, false);
        }
        case Type::I32: {
            auto nv = asNum2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : (int64_t)std::get<uint64_t>(nv);
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), s, true);
        }
        case Type::U32: {
            auto nv = asNum2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : (uint64_t)std::get<int64_t>(nv);
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), u, false);
        }
        case Type::I64: {
            auto nv = asNum2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : (int64_t)std::get<uint64_t>(nv);
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx_), s, true);
        }
        case Type::U64: {
            auto nv = asNum2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : (uint64_t)std::get<int64_t>(nv);
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx_), u, false);
        }
        case Type::F16: {
            const CLHalf h = std::get<CLHalf>(v.v);
            llvm::APFloat apf(llvm::APFloat::IEEEhalf(), llvm::APInt(16, h.bits));
            return llvm::ConstantFP::get(llvm::Type::getHalfTy(ctx_), apf);
        }

        case Type::UNIT:
		case Type::NORETURN:
			throw std::runtime_error("toLlvmConst: unit/noreturn has no value");
		case Type::STRING:
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