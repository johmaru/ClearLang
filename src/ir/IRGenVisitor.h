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

class ir_gen_visitor : public ClearLanguageBaseVisitor {
    public:
        explicit ir_gen_visitor(llvm::LLVMContext& ctx, const std::string& module_name);

        [[nodiscard]] llvm::Module& module() const;

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
        [[nodiscard]] llvm::Type* to_llvm_type(const type_ref& t) const;
        [[nodiscard]] llvm::Constant* to_llvm_constant(const value& v) const;

        llvm::Value* emit_f16_bin_op(const std::string& op, llvm::Value* lhs, llvm::Value* rhs) const;

        static llvm::Function* declare_function(const function_value& fv);
        llvm::Function* get_function_by_name(const std::string& name);
        void enter_function(llvm::Function* fn);
        void leave_function();

        llvm::Value* ensure_integer_bin_op(llvm::Value* lhs, llvm::Value* rhs, const std::string& op) const;
        [[noreturn]] static void not_implemented(const std::string& what);

    private:
        llvm::LLVMContext& ctx_;
        std::unique_ptr<llvm::Module> module_;
        std::unique_ptr<llvm::IRBuilder<>> builder_;

        std::vector<std::unordered_map<std::string, llvm::Value*>> var_scopes_;

        std::unordered_map<std::string, llvm::Function*> function_;
};