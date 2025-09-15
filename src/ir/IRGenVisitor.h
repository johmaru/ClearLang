#pragma once
#include <llvm/IR/Value.h>
#include <memory>
#include <string>
#include <unordered_map>
#include "antlr4-runtime.h"
#include "ClearLanguageBaseVisitor.h"
#include "../core/CLType.h"

// LLVM
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>

class IRGenVisitor : public ClearLanguageBaseVisitor {
    public:
        explicit IRGenVisitor(llvm::LLVMContext& ctx, const std::string& moduleName);

        llvm::Module& module();

        // entry point
        std::any visitStart(ClearLanguageParser::StartContext* ctx) override;

        // expressions
        std::any visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) override;
        std::any visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) override;
        std::any visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) override;
        std::any visitAddExpr(ClearLanguageParser::AddExprContext* ctx) override;
        std::any visitMulExpr(ClearLanguageParser::MulExprContext* ctx) override;
        std::any visitVarRef(ClearLanguageParser::VarRefContext *ctx) override;
        std::any visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) override;

        // statements
        std::any visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) override;
        std::any visitBlock(ClearLanguageParser::BlockContext* ctx) override;
        std::any visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) override;
        std::any visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) override;


    private:

        llvm::Type* toLlvmType(const TypeRef& t);
        llvm::Constant* toLlvmConstant(const Value& v);

        llvm::Value* emitF16BinOp(const std::string& op, llvm::Value* lhs, llvm::Value* rhs);

        llvm::Function* declareFunction(const FunctionValue& fv);
        llvm::Function* getFunctionByName(const std::string& name);
        void enterFunction(llvm::Function* fn);
        void leaveFunction();

        llvm::Value* ensureIntegerBinOp(llvm::Value* lhs, llvm::Value* rhs, const std::string& op);
        [[noreturn]] void notImplemented(const std::string& what);

    private:
        llvm::LLVMContext& ctx_;
        std::unique_ptr<llvm::Module> module_;
        std::unique_ptr<llvm::IRBuilder<>> builder_;

        std::vector<std::unordered_map<std::string, llvm::Value*>> varScopes_;

        std::unordered_map<std::string, llvm::Function*> function_;
};