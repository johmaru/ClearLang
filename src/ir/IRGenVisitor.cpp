#include "IRGenVisitor.h"
#include "ClearLanguageParser.h"
#include "../core/CLType.h"
#include <llvm/ADT/APFloat.h>
#include <llvm/IR/Constants.h>
#include <stdexcept>
#include <variant>

ir_gen_visitor::ir_gen_visitor(llvm::LLVMContext& ctx, const std::string& module_name)
    : ctx_(ctx), 
    module_(std::make_unique<llvm::Module>(module_name, ctx_)),
    builder_(std::make_unique<llvm::IRBuilder<>>(ctx_)) {}

llvm::Module& ir_gen_visitor::module() const {return *module_;}

std::any ir_gen_visitor::visitStart(ClearLanguageParser::StartContext* ctx) {

    not_implemented("visitStart");
}

std::any ir_gen_visitor::visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) {

    const auto txt = ctx->INT()->getText();
    const int64_t v = std::stoll(txt);
    auto* ty = llvm::Type::getInt32Ty(ctx_);
    return static_cast<llvm::Value*>(llvm::ConstantInt::get(ty, v, true));
}

std::any ir_gen_visitor::visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) {

    const auto txt = ctx->FLOAT()->getText();
    float f = std::stof(txt);
    
    llvm::APFloat apf(llvm::APFloat::IEEEhalf(), llvm::APInt(16, 0));

    not_implemented("visitFloatLiteral");
}

std::any ir_gen_visitor::visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) {
    not_implemented("visitUnaryMinus");
}

std::any ir_gen_visitor::visitAddExpr(ClearLanguageParser::AddExprContext* ctx) {
    not_implemented("visitAddExpr");
}

std::any ir_gen_visitor::visitMulExpr(ClearLanguageParser::MulExprContext* ctx) {
    not_implemented("visitMulExpr");
}

std::any ir_gen_visitor::visitVarRef(ClearLanguageParser::VarRefContext* ctx) {
    not_implemented("visitVarRef");
}

std::any ir_gen_visitor::visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) {
    return visit(ctx->expr());
}

std::any ir_gen_visitor::visitBlock(ClearLanguageParser::BlockContext* ctx) {
    not_implemented("visitBlock");
}

std::any ir_gen_visitor::visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) {
    not_implemented("visitStmtVarDecl");
}

std::any ir_gen_visitor::visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) {
    not_implemented("visitStmtReturn");
}

std::any ir_gen_visitor::visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) {
    not_implemented("visitPostfixExpr");
}

llvm::Type* ir_gen_visitor::to_llvm_type(const type_ref& t) const {
    if (!t.is_builtin()) not_implemented("toLlvmType: non-builtin type");
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
        case type::kind_enum::f16:
            return llvm::Type::getHalfTy(ctx_);
        case type::kind_enum::unit:
        case type::kind_enum::noreturn:
            return llvm::Type::getVoidTy(ctx_);
    }
    llvm_unreachable("toLlvmType: unknown kind");
}

llvm::Constant* ir_gen_visitor::to_llvm_constant(const value& v) const {
    if (!v.type.is_builtin()) not_implemented("toLlvmConstant: non-builtin type");
    switch (v.type.builtin.kind) {
        case type::kind_enum::i8: {
            auto* ty8 = llvm::Type::getInt8Ty(ctx_);
            auto nv = as_num2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(ty8, s, true);
        }
        case type::kind_enum::u8: {
            auto* ty8 = llvm::Type::getInt8Ty(ctx_);
            auto nv = as_num2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(ty8, u, false);
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
            auto* ty32 = llvm::Type::getInt32Ty(ctx_);
            auto nv = as_num2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(ty32, s, true);
        }
        case type::kind_enum::u32: {
            auto* ty32 = llvm::Type::getInt32Ty(ctx_);
            auto nv = as_num2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(ty32, u, false);
        }
        case type::kind_enum::i64: {
            auto* ty64 = llvm::Type::getInt64Ty(ctx_);
            auto nv = as_num2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(ty64, s, true);
        }
        case type::kind_enum::u64: {
            auto* ty64 = llvm::Type::getInt64Ty(ctx_);
            auto nv = as_num2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(ty64, u, false);
        }
        case type::kind_enum::f16: {
            if (!std::holds_alternative<cl_half>(v.v)) not_implemented("toLlvmConstant: non-F16 type");
            const cl_half h = std::get<cl_half>(v.v);
            llvm::APFloat apf(llvm::APFloat::IEEEhalf(), llvm::APInt(16, h.bits));
            return llvm::ConstantFP::get(llvm::Type::getHalfTy(ctx_), apf);
        }
        case type::kind_enum::unit:
        case type::kind_enum::noreturn:
            not_implemented("toLlvmConstant: UNIT/NORETURN have no value representation");
    }
	llvm_unreachable("toLlvmConstant: unknown kind");
}

llvm::Value* ir_gen_visitor::emit_f16_bin_op(const std::string& op, llvm::Value* lhs, llvm::Value* rhs) const {
    if (op == "+") return builder_->CreateFAdd(lhs, rhs, "f16addtmp");
    if (op == "-") return builder_->CreateFSub(lhs, rhs, "f16subtmp");
    if (op == "*") return builder_->CreateFMul(lhs, rhs, "f16multmp");
    if (op == "/") return builder_->CreateFDiv(lhs, rhs, "f16divtmp");
    throw std::runtime_error("unsupported f16 binary operator: " + op);
}

llvm::Function* ir_gen_visitor::declare_function(const function_value& fv) {
    not_implemented("declareFunction");
}
llvm::Function* ir_gen_visitor::get_function_by_name(const std::string& name) {
    const auto it = function_.find(name);
    return it == function_.end() ? nullptr : it->second;
}
void ir_gen_visitor::enter_function(llvm::Function* fn) {
    var_scopes_.emplace_back();
}
void ir_gen_visitor::leave_function() {
    if (!var_scopes_.empty()) var_scopes_.pop_back();
}

llvm::Value* ir_gen_visitor::ensure_integer_bin_op(llvm::Value* lhs, llvm::Value* rhs, const std::string& op) const {
    if (!lhs->getType()->isIntegerTy() || !rhs->getType()->isIntegerTy()) {
        throw std::runtime_error("integer binary operator applied to non-integer operands");
    }
    if (lhs->getType() != rhs->getType()) {
        throw std::runtime_error("integer binary operator applied to mismatched operand types");
    }
    if (op == "+") return builder_->CreateAdd(lhs, rhs, "addtmp");
    if (op == "-") return builder_->CreateSub(lhs, rhs, "subtmp");
    if (op == "*") return builder_->CreateMul(lhs, rhs, "multmp");
    if (op == "/") return builder_->CreateSDiv(lhs, rhs, "divtmp");
    throw std::runtime_error("unsupported integer binary operator: " + op);
}
[[noreturn]] void ir_gen_visitor::not_implemented(const std::string& what) {
    throw std::runtime_error("not implemented: " + what);
}