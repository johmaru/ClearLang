#include "IRGenVisitor.h"
#include "ClearLanguageParser.h"
#include "../core/CLType.h"
#include <llvm/ADT/APFloat.h>
#include <llvm/IR/Constants.h>
#include <stdexcept>
#include <variant>

IRGenVisitor::IRGenVisitor(llvm::LLVMContext& ctx, const std::string& moduleName)
    : ctx_(ctx), 
    module_(std::make_unique<llvm::Module>(moduleName, ctx_)),
    builder_(std::make_unique<llvm::IRBuilder<>>(ctx_)) {}

llvm::Module& IRGenVisitor::module() {return *module_;}

std::any IRGenVisitor::visitStart(ClearLanguageParser::StartContext* ctx) {

    notImplemented("visitStart");
}

std::any IRGenVisitor::visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) {

    auto txt = ctx->INT()->getText();
    int64_t v = std::stoll(txt);
    auto* ty = llvm::Type::getInt32Ty(ctx_);
    return static_cast<llvm::Value*>(llvm::ConstantInt::get(ty, v, true));
}

std::any IRGenVisitor::visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) {

    auto txt = ctx->FLOAT()->getText();
    float f = std::stof(txt);
    
    llvm::APFloat apf(llvm::APFloat::IEEEhalf(), llvm::APInt(16, 0));

    notImplemented("visitFloatLiteral");
}

std::any IRGenVisitor::visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) {
    notImplemented("visitUnaryMinus");
}

std::any IRGenVisitor::visitAddExpr(ClearLanguageParser::AddExprContext* ctx) {
    notImplemented("visitAddExpr");
}

std::any IRGenVisitor::visitMulExpr(ClearLanguageParser::MulExprContext* ctx) {
    notImplemented("visitMulExpr");
}

std::any IRGenVisitor::visitVarRef(ClearLanguageParser::VarRefContext* ctx) {
    notImplemented("visitVarRef");
}

std::any IRGenVisitor::visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) {
    return visit(ctx->expr());
}

std::any IRGenVisitor::visitBlock(ClearLanguageParser::BlockContext* ctx) {
    notImplemented("visitBlock");
}

std::any IRGenVisitor::visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) {
    notImplemented("visitStmtVarDecl");
}

std::any IRGenVisitor::visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) {
    notImplemented("visitStmtReturn");
}

std::any IRGenVisitor::visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) {
    notImplemented("visitPostfixExpr");
}

llvm::Type* IRGenVisitor::toLlvmType(const TypeRef& t) {
    if (!t.isBuiltin()) notImplemented("toLlvmType: non-builtin type");
    switch (t.builtin.Kind) {
        case Type::I8:  return llvm::Type::getInt8Ty(ctx_);
        case Type::U8:  return llvm::Type::getInt8Ty(ctx_);
        case Type::I32: return llvm::Type::getInt32Ty(ctx_);
        case Type::U32: return llvm::Type::getInt32Ty(ctx_);
        case Type::I64: return llvm::Type::getInt64Ty(ctx_);
        case Type::U64: return llvm::Type::getInt64Ty(ctx_);
        case Type::F16: return llvm::Type::getHalfTy(ctx_);
        case Type::UNIT: return llvm::Type::getVoidTy(ctx_);
        case Type::NORETURN: return llvm::Type::getVoidTy(ctx_);
        default: notImplemented("toLlvmType: unknown kind");
    }
}

llvm::Constant* IRGenVisitor::toLlvmConstant(const Value& v) {
    if (!v.type.isBuiltin()) notImplemented("toLlvmConstant: non-builtin type");
    switch (v.type.builtin.Kind) {
        case Type::I8: {
            auto* ty8 = llvm::Type::getInt8Ty(ctx_);
            auto nv = asNum2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(ty8, s, true);
        }
        case Type::U8: {
            auto* ty8 = llvm::Type::getInt8Ty(ctx_);
            auto nv = asNum2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(ty8, u, false);
        }
        case Type::I32: {
            auto* ty32 = llvm::Type::getInt32Ty(ctx_);
            auto nv = asNum2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(ty32, s, true);
        }
        case Type::U32: {
            auto* ty32 = llvm::Type::getInt32Ty(ctx_);
            auto nv = asNum2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(ty32, u, false);
        }
        case Type::I64: {
            auto* ty64 = llvm::Type::getInt64Ty(ctx_);
            auto nv = asNum2(v);
            int64_t s = std::holds_alternative<int64_t>(nv) ? std::get<int64_t>(nv) : static_cast<int64_t>(std::get<uint64_t>(nv));
            return llvm::ConstantInt::get(ty64, s, true);
        }
        case Type::U64: {
            auto* ty64 = llvm::Type::getInt64Ty(ctx_);
            auto nv = asNum2(v);
            uint64_t u = std::holds_alternative<uint64_t>(nv) ? std::get<uint64_t>(nv) : static_cast<uint64_t>(std::get<int64_t>(nv));
            return llvm::ConstantInt::get(ty64, u, false);
        }
        case Type::F16: {
            if (!std::holds_alternative<CLHalf>(v.v)) notImplemented("toLlvmConstant: non-F16 type");
            const CLHalf h = std::get<CLHalf>(v.v);
            llvm::APFloat apf(llvm::APFloat::IEEEhalf(), llvm::APInt(16, h.bits));
            return llvm::ConstantFP::get(llvm::Type::getHalfTy(ctx_), apf);
        }
        default: notImplemented("toLlvmConstant: unsupported type");
    }
}

llvm::Value* IRGenVisitor::emitF16BinOp(const std::string& op, llvm::Value* lhs, llvm::Value* rhs) {
    if (op == "+") return builder_->CreateFAdd(lhs, rhs, "f16addtmp");
    if (op == "-") return builder_->CreateFSub(lhs, rhs, "f16subtmp");
    if (op == "*") return builder_->CreateFMul(lhs, rhs, "f16multmp");
    if (op == "/") return builder_->CreateFDiv(lhs, rhs, "f16divtmp");
    throw std::runtime_error("unsupported f16 binary operator: " + op);
}

llvm::Function* IRGenVisitor::declareFunction(const FunctionValue& fv) {
    notImplemented("declareFunction");
}
llvm::Function* IRGenVisitor::getFunctionByName(const std::string& name) {
    auto it = function_.find(name);
    return it == function_.end() ? nullptr : it->second;
}
void IRGenVisitor::enterFunction(llvm::Function* fn) {
    varScopes_.emplace_back();
}
void IRGenVisitor::leaveFunction() {
    if (!varScopes_.empty()) varScopes_.pop_back();
}

llvm::Value* IRGenVisitor::ensureIntegerBinOp(llvm::Value* lhs, llvm::Value* rhs, const std::string& op) {
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
[[noreturn]] void IRGenVisitor::notImplemented(const std::string& what) {
    throw std::runtime_error("not implemented: " + what);
}