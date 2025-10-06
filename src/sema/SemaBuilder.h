#pragma once
#include "antlr4-runtime.h"
#include "ClearLanguageBaseVisitor.h"
#include "SemaIR.h"
#include <memory>

class sema_builder : public ClearLanguageBaseVisitor {
    public :
        sema_builder();

        std::shared_ptr<sema::module> take_module();

        std::any visitStart(ClearLanguageParser::StartContext* ctx) override;
        std::any visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) override;
        std::any visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) override;
        std::any visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) override;
        std::any visitUnaryPrimary(ClearLanguageParser::UnaryPrimaryContext* ctx) override;
        std::any visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) override;
        std::any visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) override;
        std::any visitUnitLiteral(ClearLanguageParser::UnitLiteralContext* ctx) override;
		std::any visitOrExpr(ClearLanguageParser::OrExprContext* ctx) override;
		std::any visitAndExpr(ClearLanguageParser::AndExprContext* ctx) override;
		std::any visitEqualExpr(ClearLanguageParser::EqualExprContext* ctx) override;
        std::any visitAddExpr(ClearLanguageParser::AddExprContext* ctx) override;
        std::any visitMulExpr(ClearLanguageParser::MulExprContext* ctx) override;
        std::any visitVarRef(ClearLanguageParser::VarRefContext* ctx) override;
        std::any visitBlock(ClearLanguageParser::BlockContext* ctx) override;
		std::any visitIfBlock(ClearLanguageParser::IfBlockContext* ctx) override;
		std::any visitIfSingle(ClearLanguageParser::IfSingleContext* ctx) override;
        std::any visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) override;
        std::any visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) override;
		std::any visitStringLiteral(ClearLanguageParser::StringLiteralContext* ctx) override;
		std::any visitBoolLiteral(ClearLanguageParser::BoolLiteralContext* ctx) override;
		std::any visitStmtExpr(ClearLanguageParser::StmtExprContext* ctx) override;
    private:
        std::string current_package_;

        [[nodiscard]] std::string qualify(const std::string& name) const {
            return current_package_.empty() ? name : current_package_ + "::" + name;
		}

        std::unordered_map<std::string, std::string> imports_;

        [[nodiscard]] std::string resolve_function_name(const std::string& name) const;

        [[nodiscard]] type_ref resolve_type(const std::string& name) const;
        static type_ref make_type_ref_from(ClearLanguageParser::TypeContext* ctx);

        std::shared_ptr<sema::module> mod_;

        std::vector<std::unordered_map<std::string, type_ref>> type_scopes_;
        std::vector<std::unordered_map<std::string, type_ref>> var_types_;

        std::unordered_map<std::string, std::shared_ptr<function_sig>> func_sigs_;
        type_ref current_return_type_ = type_ref::builtin_type(type{type::kind_enum::unit});
};