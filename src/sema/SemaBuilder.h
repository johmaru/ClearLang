#pragma once
#include "antlr4-runtime.h"
#include "ClearLanguageBaseVisitor.h"
#include "SemaIR.h"
#include <memory>

class SemaBuilder : public ClearLanguageBaseVisitor {
    public :
        SemaBuilder();

        std::shared_ptr<sema::Module> takeModule();

        std::any visitStart(ClearLanguageParser::StartContext* ctx) override;
        std::any visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) override;
        std::any visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) override;
        std::any visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) override;
        std::any visitUnaryPrimary(ClearLanguageParser::UnaryPrimaryContext* ctx) override;
        std::any visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) override;
        std::any visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) override;
        std::any visitUnitLiteral(ClearLanguageParser::UnitLiteralContext* ctx) override;
        std::any visitAddExpr(ClearLanguageParser::AddExprContext* ctx) override;
        std::any visitMulExpr(ClearLanguageParser::MulExprContext* ctx) override;
        std::any visitVarRef(ClearLanguageParser::VarRefContext* ctx) override;
        std::any visitBlock(ClearLanguageParser::BlockContext* ctx) override;
        std::any visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) override;
        std::any visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) override;

    private:
        TypeRef resolveType(const std::string& name) const;
        TypeRef makeTypeRefFrom(ClearLanguageParser::TypeContext* ctx);

        std::shared_ptr<sema::Module> mod_;

        std::vector<std::unordered_map<std::string, TypeRef>> typeScopes_;
        std::vector<std::unordered_map<std::string, TypeRef>> varTypes_;

        std::unordered_map<std::string, std::shared_ptr<FunctionSig>> funcSigs_;
        TypeRef currentReturnType_ = TypeRef::builtinType(Type{Type::UNIT});
};