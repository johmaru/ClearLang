#include "SemaBuilder.h"
#include "ClearLanguageParser.h"
#include "../core/CLType.h"
#include "../core/SemaUtils.h"
#include "SemaIR.h"
#include <any>
#include <memory>
#include <stdexcept>
#include <string>
#include <llvm/ADT/APFloat.h>

using sema::expr; using sema::literal; using sema::var_ref;
using sema::unary; using sema::bin_op; using sema::block;
using sema::stmt_var_decl; using sema::stmt_return; using sema::function;

namespace {
	bool is_int_kind(const type::kind_enum k) {
        switch (k) {
	        case type::kind_enum::i8: case type::kind_enum::u8:
	        case type::kind_enum::i16: case type::kind_enum::u16:
	        case type::kind_enum::i32: case type::kind_enum::u32:
	        case type::kind_enum::i64: case type::kind_enum::u64:
	            return true;
	        default: return false;
        }
    }

    bool is_bool_kind(const type::kind_enum k) {
	    switch (k) {
	    case type::kind_enum::boolean:
            return true;

        default: return false;
	    }
	}

    bool is_num_kind(const type::kind_enum k) {
        switch (k) {
	        case type::kind_enum::i8: case type::kind_enum::u8:
	        case type::kind_enum::i16: case type::kind_enum::u16:
	        case type::kind_enum::i32: case type::kind_enum::u32:
	        case type::kind_enum::i64: case type::kind_enum::u64:
	        case type::kind_enum::f16: case type::kind_enum::f32:
	            return true;
	        default: return false;
        }
    }

    type_ref boolean_type() {
        return type_ref::builtin_type(type{ type::kind_enum::boolean });
    }
}

sema_builder::sema_builder() : mod_(std::make_shared<sema::module>()) {
    type_scopes_.emplace_back();
    type_scopes_.back().emplace("i8", type_ref::builtin_type(type{type::kind_enum::i8}));
    type_scopes_.back().emplace("u8", type_ref::builtin_type(type{type::kind_enum::u8}));
    type_scopes_.back().emplace("i16", type_ref::builtin_type(type{ type::kind_enum::i16 }));
	type_scopes_.back().emplace("u16", type_ref::builtin_type(type{ type::kind_enum::u16 }));
    type_scopes_.back().emplace("i32", type_ref::builtin_type(type{type::kind_enum::i32}));
    type_scopes_.back().emplace("int", type_ref::builtin_type(type{type::kind_enum::i32}));
    type_scopes_.back().emplace("u32", type_ref::builtin_type(type{type::kind_enum::u32}));
    type_scopes_.back().emplace("i64", type_ref::builtin_type(type{type::kind_enum::i64}));
    type_scopes_.back().emplace("u64", type_ref::builtin_type(type{type::kind_enum::u64}));
    type_scopes_.back().emplace("f16", type_ref::builtin_type(type{type::kind_enum::f16}));
    type_scopes_.back().emplace("f32", type_ref::builtin_type(type{ type::kind_enum::f32 }));
    type_scopes_.back().emplace("noreturn", type_ref::builtin_type(type{type::kind_enum::noreturn}));
    type_scopes_.back().emplace("unit", type_ref::builtin_type(type{type::kind_enum::unit}));
	type_scopes_.back().emplace("()", type_ref::builtin_type(type{ type::kind_enum::unit }));
	type_scopes_.back().emplace("string", type_ref::builtin_type(type{ type::kind_enum::string }));
    type_scopes_.back().emplace("bool", type_ref::builtin_type(type{ type::kind_enum::boolean }));
    var_types_.emplace_back();


	// Signatures of built-in functions
    {
		const auto sig = std::make_shared<function_sig>();
		sig->param_types.push_back(type_ref::builtin_type(type{ type::kind_enum::string }));
		sig->return_type = std::make_shared<type_ref>(type_ref::builtin_type(type{ type::kind_enum::unit }));
		func_sigs_["__cl_printf"] = sig;
    }

    {
        const auto sig = std::make_shared<function_sig>();
        sig->param_types.push_back(type_ref::builtin_type(type{ type::kind_enum::i8 }));
        sig->return_type = std::make_shared<type_ref>(type_ref::builtin_type(type{ type::kind_enum::unit }));
        func_sigs_["__cl_i8_printf"] = sig;
    }

    {
        const auto sig = std::make_shared<function_sig>();
        sig->param_types.push_back(type_ref::builtin_type(type{ type::kind_enum::i8 }));
        sig->return_type = std::make_shared<type_ref>(type_ref::builtin_type(type{ type::kind_enum::unit }));
        func_sigs_["__cl_i8_printfn"] = sig;
    }

    {
        const auto sig = std::make_shared<function_sig>();
        sig->param_types.push_back(type_ref::builtin_type(type{ type::kind_enum::u8 }));
        sig->return_type = std::make_shared<type_ref>(type_ref::builtin_type(type{ type::kind_enum::unit }));
        func_sigs_["__cl_u8_printfn"] = sig;
    }

    {
        const auto sig = std::make_shared<function_sig>();
        sig->param_types.push_back(type_ref::builtin_type((type{ type::kind_enum::i16 })));
        sig->return_type = std::make_shared<type_ref>(type_ref::builtin_type(type{ type::kind_enum::unit }));
        func_sigs_["__cl_i16_printfn"] = sig;
    }

    {
        const auto sig = std::make_shared<function_sig>();
        sig->param_types.push_back(type_ref::builtin_type((type{ type::kind_enum::f16 })));
        sig->return_type = std::make_shared<type_ref>(type_ref::builtin_type(type{ type::kind_enum::unit }));
        func_sigs_["__cl_f16_printfn"] = sig;
    }

    {
        const auto sig = std::make_shared<function_sig>();
        sig->param_types.push_back(type_ref::builtin_type((type{ type::kind_enum::f32 })));
        sig->return_type = std::make_shared<type_ref>(type_ref::builtin_type(type{ type::kind_enum::unit }));
        func_sigs_["__cl_f32_printfn"] = sig;
    }

    {
        auto addParse = [&](const char* name, type::kind_enum k) {

            const auto sig = std::make_shared<function_sig>();
            sig->param_types.push_back(type_ref::builtin_type(type{ type::kind_enum::string }));
			sig->return_type = std::make_shared<type_ref>(type_ref::builtin_type(type{ k }));
			func_sigs_[name] = sig;
        };
		addParse("__cl_parse_i8", type::kind_enum::i8);
		addParse("__cl_parse_u8", type::kind_enum::u8);
		addParse("__cl_parse_i16", type::kind_enum::i16);
		addParse("__cl_parse_u16", type::kind_enum::u16);
		addParse("__cl_parse_i32", type::kind_enum::i32);
		addParse("__cl_parse_u32", type::kind_enum::u32);
		addParse("__cl_parse_i64", type::kind_enum::i64);
		addParse("__cl_parse_u64", type::kind_enum::u64);
    }
}

std::shared_ptr<sema::module> sema_builder::take_module() { return std::move(mod_); }

type_ref sema_builder::resolve_type(const std::string& name) const {
    for (auto it = type_scopes_.rbegin(); it != type_scopes_.rend(); ++it) {
        auto f = it->find(name);
        if (f != it->end()) return f->second;
    }
    return type_ref::builtin_type(type::from_string(name));
}

type_ref sema_builder::make_type_ref_from(ClearLanguageParser::TypeContext* ctx) {
    if (const auto nt = dynamic_cast<ClearLanguageParser::NamedTypeContext*>(ctx)) {
        return type_ref::builtin_type(type::from_string(nt->IDENT()->getText()));
    }
    if (dynamic_cast<ClearLanguageParser::UnitTypeContext*>(ctx)) {
	    return type_ref::builtin_type(type{type::kind_enum::unit});
    }
    if (const auto ft = dynamic_cast<ClearLanguageParser::FunctionTypeContext*>(ctx)) {
	    auto sig = std::make_shared<function_sig>();
	    if (const auto tl = ft->typeList()) {
		    for (auto* tctx : tl->type()) sig->param_types.push_back(make_type_ref_from(tctx));
	    }
	    sig->return_type = std::make_shared<type_ref>(make_type_ref_from(ft->type()));
	    return type_ref::function_type(std::move(sig));
    }
    throw std::runtime_error("unknown type alt");
}

std::any sema_builder::visitStart(ClearLanguageParser::StartContext* ctx) {

    for (auto* fd : ctx->funcDecl()) {
        auto name = fd->name->getText();
        const auto sig = std::make_shared<function_sig>();
        if (const auto pl = fd->paramList()) {
            for (auto* p : pl->param()) {
                sig->param_types.push_back(make_type_ref_from(p->type()));
            }
        }
        sig->return_type = std::make_shared<type_ref>(make_type_ref_from(fd->type()));
        func_sigs_[name] = sig;

        // pick up entry point name if any
        for (auto* at : fd->attributes()) {
            for (auto* id : at->IDENT()) {
                if (id->getText() == "EntryPoint") {
                    mod_->entry_name = name;
                }
            }
        }
    }

    for (auto* fd : ctx->funcDecl()) {
        auto fun = std::make_shared<function>();
        fun->name = fd->name->getText();
        if (const auto pl = fd->paramList()) {
            for (auto* p : pl->param()) {
                sema::param prm;
                prm.name = p->IDENT()->getText();
                prm.type = make_type_ref_from(p->type());
                fun->params.push_back(std::move(prm));
            }
        }
        fun->return_type = make_type_ref_from(fd->type());
        fun->body = std::make_shared<block>();

        current_return_type_ = fun->return_type;
        var_types_.emplace_back();
        for (auto& prm : fun->params) var_types_.back()[prm.name] = prm.type;

        auto any_blk = visit(fd->block());
        *fun->body = *std::any_cast<std::shared_ptr<block>>(any_blk);
        var_types_.pop_back();
        mod_->functions.push_back(std::move(fun));
    }
    return nullptr;
}

std::any sema_builder::visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) {
    const auto node = std::make_shared<sema::literal>();
    node->type = type_ref::builtin_type(type{type::kind_enum::i32});
    int64_t v = std::stoll(ctx->INT()->getText());
    node->value = value{node->type, v, true};
    return std::static_pointer_cast<expr>(node);
}

std::any sema_builder::visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) {
    const auto node = std::make_shared<literal>();
    node->type = type_ref::builtin_type(type{type::kind_enum::f32});
    const std::string tok = ctx->FLOAT()->getText();
    const llvm::APFloat ap(llvm::APFloat::IEEEsingle(), tok);
    if (ap.isInfinity()) throw std::runtime_error("f32: Out of range" + tok);
    const uint32_t bits = static_cast<uint32_t>(ap.bitcastToAPInt().getZExtValue());
    cl_f32 h; h.bits = bits;

    node->value = value(node->type, h, false);
    return std::static_pointer_cast<expr>(node);
}

std::any sema_builder::visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) {
    const auto inner = std::any_cast<std::shared_ptr<expr>>(visit(ctx->inner));

    const auto node = std::make_shared<unary>();
    node->op = "-";
    node->inner = inner;
    node->type = inner->type;
    return std::static_pointer_cast<expr>(node);
}

std::any sema_builder::visitOrExpr(ClearLanguageParser::OrExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));

    if (ctx->right.empty()) return cur;

    if (!(cur->type.is_builtin() && is_int_kind(cur->type.builtin.kind))) throw std::runtime_error("operator 'or' expects integer operands (left)");

    for (size_t i = 0; i < ctx->right.size(); i++) {
        const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
        if (!(rhs->type.is_builtin() && is_int_kind(rhs->type.builtin.kind))) throw std::runtime_error("operator or expects integer operands (right)");

        const auto node = std::make_shared<bin_op>();
        node->op = "or";
        node->lhs = cur;
        node->rhs = rhs;
        node->type = boolean_type();
        cur = node;
    }
    return cur;
}

std::any sema_builder::visitAndExpr(ClearLanguageParser::AndExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));

    if (ctx->right.empty()) return cur;

    if (!(cur->type.is_builtin() && is_int_kind(cur->type.builtin.kind))) throw std::runtime_error("operator 'and' expects integer operands (left)");

    for (size_t i = 0; i < ctx->right.size(); i++) {
        const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
        if (!(rhs->type.is_builtin() && is_int_kind(rhs->type.builtin.kind))) throw std::runtime_error("operator 'and' expects integer operands (right)");

        const auto node = std::make_shared<bin_op>();
        node->op = "and";
        node->lhs = cur;
        node->rhs = rhs;
        node->type = boolean_type();
        cur = node;
    }
    return cur;
}

std::any sema_builder::visitEqualExpr(ClearLanguageParser::EqualExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));

    for (size_t i = 0; i < ctx->right.size(); i++) {
        const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
        const std::string op = ctx->op[i]->getText();
        if (!(cur->type.is_builtin() && rhs->type.is_builtin())) throw std::runtime_error("equal operator requires builtin types");
        const auto lk = cur->type.builtin.kind;
        const auto rk = rhs->type.builtin.kind;

        if (!(is_num_kind(lk) && lk == rk)) throw std::runtime_error("type mismatch in equal op (only num same types)");

        const auto node = std::make_shared<bin_op>();
        node->op = op;
        node->lhs = cur;
        node->rhs = rhs;
        node->type = boolean_type();
        cur = node;
    }
    return cur;
}



std::any sema_builder::visitAddExpr(ClearLanguageParser::AddExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));
    for (size_t i = 0; i < ctx->right.size(); ++i) {
        const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
        std::string op = ctx->op[i]->getText();
        if (!(cur->type.is_builtin() && rhs->type.is_builtin() &&
              cur->type.builtin.kind == rhs->type.builtin.kind)) {
            throw std::runtime_error("type mismatch in binary op");
        }
        if (is_string(cur->type) && is_string(rhs->type) ) {
	        if (op != "+") {
                throw std::runtime_error("only + operator is supported for string concatenation");
			}
        }
        const auto bin = std::make_shared<bin_op>();
        bin->op = op;
        bin->lhs = cur;
        bin->rhs = rhs;
        bin->type = cur->type;
        cur = bin;
    }
    return cur;
}

std::any sema_builder::visitMulExpr(ClearLanguageParser::MulExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));
    for (size_t i = 0; i < ctx->right.size(); ++i) {
        const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
        const std::string op = ctx->op[i]->getText();
        if (!(cur->type.is_builtin() && rhs->type.is_builtin() &&
              cur->type.builtin.kind == rhs->type.builtin.kind)) {
            throw std::runtime_error("type mismatch in binary op");
        }
        const auto bin = std::make_shared<bin_op>();
        bin->op = op;
        bin->lhs = cur;
        bin->rhs = rhs;
        bin->type = cur->type;
        cur = bin;
    }
    return cur;
}

std::any sema_builder::visitVarRef(ClearLanguageParser::VarRefContext* ctx) {
    const auto node = std::make_shared<var_ref>();
    node->name = ctx->IDENT()->getText();
    for (auto it = var_types_.rbegin(); it != var_types_.rend(); ++it) {
        auto f = it->find(node->name);
        if (f != it->end()) { node->type = f->second; return std::static_pointer_cast<expr>(node); }
    }

    if (const auto fit = func_sigs_.find(node->name); fit != func_sigs_.end()) {
        node->type = type_ref::function_type(fit->second);
        return std::static_pointer_cast<expr>(node);
    }
    throw std::runtime_error("undefined variable: " + node->name);
}

std::any sema_builder::visitBlock(ClearLanguageParser::BlockContext* ctx) {
    auto blk = std::make_shared<block>();
    for (auto* s : ctx->stmt()) {
        if (auto* sr = dynamic_cast<ClearLanguageParser::StmtReturnContext*>(s)) {
            blk->statements.push_back(std::any_cast<std::shared_ptr<stmt_return>>(visit(sr)));
        } else if (auto* vd = dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(s)) {
            blk->statements.push_back(std::any_cast<std::shared_ptr<stmt_var_decl>>(visit(vd)));
        } else if (auto* se = dynamic_cast<ClearLanguageParser::StmtExprContext*>(s)) {
			blk->statements.push_back(std::any_cast<std::shared_ptr<sema::stmt_expr>>(visit(se)));
        } else if (auto* si = dynamic_cast<ClearLanguageParser::StmtIfContext*>(s)) {
            blk->statements.push_back(std::any_cast<std::shared_ptr<sema::stmt_if>>(visit(si)));
        } else {
            // currently pass expression statements
            visit(s);
        }
    }
    return blk;
}

std::any sema_builder::visitIfBlock(ClearLanguageParser::IfBlockContext* ctx) {
    const auto cond = std::any_cast<std::shared_ptr<expr>>(visit(ctx->expr()));

    const bool is_bool = cond->type.is_builtin() && is_bool_kind(cond->type.builtin.kind);
    const bool is_int = cond->type.is_builtin() && is_int_kind(cond->type.builtin.kind);

    if (!(is_bool || is_int)) throw std::runtime_error("if condition must be boolean or int expression");

    auto node = std::make_shared<sema::stmt_if>();
    node->cond = cond;
    node->then_blk = std::any_cast<std::shared_ptr<block>>(visit(ctx->block(0)));

    node->else_blk = nullptr;
    const auto blks = ctx->block();
    auto* else_stmt_ctx = ctx->stmt();
    if (blks.size() >= 2) {
        node->else_blk = std::any_cast<std::shared_ptr<block>>(visit(ctx->block(1)));
    } else if (else_stmt_ctx != nullptr) {
        const auto else_blk = std::make_shared<block>();
        if (auto* sr = dynamic_cast<ClearLanguageParser::StmtReturnContext*>(else_stmt_ctx)) {
            else_blk->statements.push_back(std::any_cast<std::shared_ptr<stmt_return>>(visit(sr)));
        }
        else if (auto* vd = dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(else_stmt_ctx)) {
            else_blk->statements.push_back(std::any_cast<std::shared_ptr<stmt_var_decl>>(visit(vd)));
        }
        else if (auto* se = dynamic_cast<ClearLanguageParser::StmtExprContext*>(else_stmt_ctx)) {
            else_blk->statements.push_back(std::any_cast<std::shared_ptr<sema::stmt_expr>>(visit(se)));
        }
        else if (auto* si = dynamic_cast<ClearLanguageParser::StmtIfContext*>(else_stmt_ctx)) {
            else_blk->statements.push_back(std::any_cast<std::shared_ptr<sema::stmt_if>>(visit(si)));
        }
        else {
            visit(else_stmt_ctx);
        }
        node->else_blk = else_blk;
    }

    return node;
}

std::any sema_builder::visitIfSingle(ClearLanguageParser::IfSingleContext* ctx) {
    const auto cond = std::any_cast<std::shared_ptr<expr>>(visit(ctx->expr()));
    
    const bool is_bool = cond->type.is_builtin() && is_bool_kind(cond->type.builtin.kind);
    const bool is_int = cond->type.is_builtin() && is_int_kind(cond->type.builtin.kind);

    if (!(is_bool || is_int)) throw std::runtime_error("if condition must be boolean or int expression");

    auto node = std::make_shared<sema::stmt_if>();
    node->cond = cond;

    const auto then_blk = std::make_shared <block>();
    auto* then_stmt_ctx = ctx->stmt(0);
    if (auto* sr = dynamic_cast<ClearLanguageParser::StmtReturnContext*>(then_stmt_ctx)) {
        then_blk->statements.push_back(std::any_cast<std::shared_ptr<stmt_return>>(visit(sr)));
    }
    else if (auto* vd = dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(then_stmt_ctx)) {
        then_blk->statements.push_back(std::any_cast<std::shared_ptr<stmt_var_decl>>(visit(vd)));
    }
    else if (auto* se = dynamic_cast<ClearLanguageParser::StmtExprContext*>(then_stmt_ctx)) {
        then_blk->statements.push_back(std::any_cast<std::shared_ptr<sema::stmt_expr>>(visit(se)));
    }
    else if (auto* si = dynamic_cast<ClearLanguageParser::StmtIfContext*>(then_stmt_ctx)) {
        then_blk->statements.push_back(std::any_cast<std::shared_ptr<sema::stmt_if>>(visit(si)));
    }
    else {
        visit(then_stmt_ctx);
    }
    node->then_blk = then_blk;

    node->else_blk = nullptr;
    const auto& blks = ctx->getRuleContexts<ClearLanguageParser::BlockContext>();
    const auto& stmts_vec = ctx->stmt();
    if (!blks.empty()) {
        node->else_blk = std::any_cast<std::shared_ptr<block>>(visit(blks[0]));
    }
    else if (stmts_vec.size() >= 2) {
    	const auto else_blk = std::make_shared<block>();
        auto* else_stmt_ctx = stmts_vec[1];
        if (auto* sr = dynamic_cast<ClearLanguageParser::StmtReturnContext*>(else_stmt_ctx)) {
            else_blk->statements.push_back(std::any_cast<std::shared_ptr<stmt_return>>(visit(sr)));
        }
        else if (auto* vd = dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(else_stmt_ctx)) {
            else_blk->statements.push_back(std::any_cast<std::shared_ptr<stmt_var_decl>>(visit(vd)));
        }
        else if (auto* se = dynamic_cast<ClearLanguageParser::StmtExprContext*>(else_stmt_ctx)) {
            else_blk->statements.push_back(std::any_cast<std::shared_ptr<sema::stmt_expr>>(visit(se)));
        }
        else if (auto* si = dynamic_cast<ClearLanguageParser::StmtIfContext*>(else_stmt_ctx)) {
            else_blk->statements.push_back(std::any_cast<std::shared_ptr<sema::stmt_if>>(visit(si)));
        }
        else {
            visit(else_stmt_ctx);
        }
        node->else_blk = else_blk;
    }
	
    return node;
}



std::any sema_builder::visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) {
    auto* vd = ctx->varDecl();
    auto node = std::make_shared<stmt_var_decl>();
    node->name = vd->IDENT()->getText();
    node->decl_type = make_type_ref_from(vd->type());

    std::shared_ptr<expr> init_expr;
    if (vd->expr()) {
        init_expr = std::any_cast<std::shared_ptr<expr>>(visit(vd->expr()));
        if (init_expr->type.is_builtin() && init_expr->type.builtin.kind == type::kind_enum::i32) {
            if (const auto lit = std::dynamic_pointer_cast<literal>(init_expr)) {
                if (lit->value.is_untyped_int) {
                    const auto coerced = sema_utils::coerce_untyped_int_to(lit->value, node->decl_type);
                    lit->value = coerced;
                    lit->type = node->decl_type;
                    init_expr->type = node->decl_type;
                }
            }
        }
        if (init_expr->type.is_builtin() &&
            init_expr->type.builtin.kind != node->decl_type.builtin.kind) {
            throw std::runtime_error("type mismatch in var init");
        }
    }
    node->init_expr = init_expr;
    
    var_types_.back().emplace(node->name, node->decl_type);
    return node;
}

std::any sema_builder::visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) {
    auto node = std::make_shared<stmt_return>();
    if (ctx->expr()) {
        node->value = std::any_cast<std::shared_ptr<expr>>(visit(ctx->expr()));
    }
    return node;
}

std::any sema_builder::visitStmtExpr(ClearLanguageParser::StmtExprContext* ctx)
{
	auto node = std::make_shared<sema::stmt_expr>();
	node->expr = std::any_cast<std::shared_ptr<expr>>(visit(ctx->expr()));
	return node;
}


std::any sema_builder::visitUnaryPrimary(ClearLanguageParser::UnaryPrimaryContext* ctx) {
    // primary -> int/float/ident/paren/unit
    return visit(ctx->postfixExpr());
}

std::any sema_builder::visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->primary()));

    auto is_int_kind = [](const type::kind_enum k) {
        switch (k) {
        case type::kind_enum::i8: case type::kind_enum::u8:
        case type::kind_enum::i16: case type::kind_enum::u16:
        case type::kind_enum::i32: case type::kind_enum::u32:
        case type::kind_enum::i64: case type::kind_enum::u64:
            return true;
        case type::kind_enum::string: case type::kind_enum::f16: case type::kind_enum::f32: case type::kind_enum::noreturn: case type::kind_enum::unit:
            return false;
        }
        return false;
        };

    auto parse_func_for = [](const type::kind_enum k) -> const char* {
	    switch (k) {
			case type::kind_enum::i8: return "__cl_parse_i8";
			case type::kind_enum::u8: return "__cl_parse_u8";
			case type::kind_enum::i16: return "__cl_parse_i16";
			case type::kind_enum::u16: return "__cl_parse_u16";
			case type::kind_enum::i32: return "__cl_parse_i32";
			case type::kind_enum::u32: return "__cl_parse_u32";
			case type::kind_enum::i64: return "__cl_parse_i64";
			case type::kind_enum::u64: return "__cl_parse_u64";
            case type::kind_enum::string: case type::kind_enum::f16: case type::kind_enum::f32: case type::kind_enum::noreturn: case type::kind_enum::unit:
			return nullptr;
	    }
		return nullptr;
    };

    for (auto* child : ctx->children) {
	    if (auto* cs = dynamic_cast<ClearLanguageParser::CallSuffixContext*>(child)) {
			auto vr = std::dynamic_pointer_cast<var_ref>(cur);
			if (!vr) throw std::runtime_error("can only call functions by name");

			auto call = std::make_shared<sema::call>();
			call->callee = vr->name;

            call->args.clear();
            if (auto al = cs->argList()) {
	            for (auto* ectx : al->expr()) {
                    call->args.push_back(std::any_cast<std::shared_ptr<expr>>(visit(ectx)));
				}
            }

			auto it = func_sigs_.find(call->callee);
			if (it == func_sigs_.end()) throw std::runtime_error("call to undefined function: " + call->callee);
			auto sig = it->second;
			if (sig->param_types.size() != call->args.size()) throw std::runtime_error("argument count mismatch in function call: " + call->callee);
            for (size_t i = 0; i < call->args.size(); ++i) {
                if (!(call->args[i]->type.is_builtin() && sig->param_types[i].is_builtin() &&
                    call->args[i]->type.builtin.kind == sig->param_types[i].builtin.kind)) {
                    throw std::runtime_error("argument type mismatch in function call: " + call->callee);
                }
            }
			call->type = *sig->return_type;

            cur = call;
            continue;
	    }

        if (auto* as = dynamic_cast<ClearLanguageParser::AsSuffixContext*>(child)) {
			type_ref target = make_type_ref_from(as->type());

            if (auto lit = std::dynamic_pointer_cast<literal>(cur)) {
                if (lit->value.is_untyped_int) {
                    auto coerced = sema_utils::coerce_untyped_int_to(lit->value, target);
                    lit->value = coerced;
                    lit->type = target;
                    cur->type = target;
                    continue;
                }
            }

            if (!(cur->type.is_builtin() && target.is_builtin())) {
                throw std::runtime_error("can only cast between builtin types");
			}

			const auto src_k = cur->type.builtin.kind;
			const auto dst_k = target.builtin.kind;

			if (src_k == dst_k) { cur->type = target; continue; }

			bool ok = false;
            if (is_int_kind(src_k) && is_int_kind(dst_k)) ok = true;
			if (is_int_kind(src_k) && dst_k == type::kind_enum::f16) ok = true;
            if (is_int_kind(src_k) && dst_k == type::kind_enum::f32) ok = true;
            if (src_k == type::kind_enum::f16 && is_int_kind(dst_k)) ok = true;
            if (src_k == type::kind_enum::f32 && is_int_kind(dst_k)) ok = true;

            if (src_k == type::kind_enum::string || dst_k == type::kind_enum::string) ok = false;

            if (!ok) {
                throw std::runtime_error(
                    std::string("unsupported 'as' cast: ") +
                    builtin_type_name(cur->type.builtin) + " -> " + builtin_type_name(target.builtin));
            }

			auto cast = std::make_shared<sema::cast>();
			cast->inner = cur;
			cast->target_type = target;
			cast->type = target;
            cur = cast;
			continue;
        }

        if (auto* asf = dynamic_cast<ClearLanguageParser::AsForceSuffixContext*>(child)) {
	        type_ref target = make_type_ref_from(asf->type());
            if (auto lit = std::dynamic_pointer_cast<literal>(cur)) {
                if (lit->value.is_untyped_int) {
                    auto coerced = sema_utils::coerce_untyped_int_to(lit->value, target);
                    lit->value = coerced;
                    lit->type = target;
                    cur->type = target;
                    continue;
                }
            }
            if (!(cur->type.is_builtin() && target.is_builtin())) {
                throw std::runtime_error("can only cast between builtin types");
            }
            const auto src_k = cur->type.builtin.kind;
            const auto dst_k = target.builtin.kind;
            if (src_k == dst_k) { cur->type = target; continue; }

            if (src_k == type::kind_enum::string && is_int_kind(dst_k)) {
	            if (auto lit = std::dynamic_pointer_cast<literal>(cur)) {
		            if (std::holds_alternative<std::string>(lit->value.v)) {
                        const std::string& s = std::get<std::string>(lit->value.v);
                        auto is_digits = [](const std::string& t) {
                            if (t.empty()) return false;
                            size_t i = (t[0] == '+' || t[0] == '-') ? 1u : 0u;
                            if (i >= t.size())return false;
                            for (; i < t.size(); ++i) if (!std::isdigit(static_cast<unsigned char>(t[i]))) return false;
                            return true;
                        };
                        if (is_digits(s)) {
	                        try {
		                        if (type_ref::is_unsigned(target)) {
                                    uint64_t u = static_cast<uint64_t>(std::stoull(s));
                                    if (!fits(target, std::variant<int64_t, uint64_t>(u)))
                                        throw std::runtime_error("overflow");
                                    lit->value = value{ target, u, false };
		                        } else {
                                    int64_t i = static_cast<int64_t>(std::stoll(s));
                                    if (!fits(target, std::variant<int64_t, uint64_t>(i)))
                                        throw std::runtime_error("overflow");
                                    lit->value = value{ target, i, false };
		                        }
                                lit->type = target;
                                cur->type = target;
                                continue;
	                        } catch (...) {
	                        }
                        }
		            }
	            }
                const char* callee = parse_func_for(dst_k);
                if (!callee) {
                    throw std::runtime_error("string to this target type via 'as! is not supported");
                }
                auto call = std::make_shared<sema::call>();
                call->callee = callee;
                call->args.clear();
                call->args.push_back(cur);
                call->type = target;
                cur = call;
                continue;
            }

            bool ok = false;
            if (is_int_kind(src_k) && is_int_kind(dst_k)) ok = true;
            if (is_int_kind(src_k) && dst_k == type::kind_enum::f16) ok = true;
            if (is_int_kind(src_k) && dst_k == type::kind_enum::f32) ok = true;
            if (src_k == type::kind_enum::f16 && is_int_kind(dst_k)) ok = true;
            if (src_k == type::kind_enum::f32 && is_int_kind(dst_k)) ok = true;

            if (src_k == type::kind_enum::string || dst_k == type::kind_enum::string) ok = false;

            if (!ok) {
                throw std::runtime_error(
                    std::string("unsupported 'as!' cast: ") +
                    builtin_type_name(cur->type.builtin) + " -> " + builtin_type_name(target.builtin));
            }

            auto cast = std::make_shared<sema::cast>();
            cast->inner = cur;
            cast->target_type = target;
            cast->type = target;
            cur = cast;
            continue;
        }

    }
	return cur;
}

std::any sema_builder::visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) {
    return visit(ctx->expr());
}

std::any sema_builder::visitUnitLiteral(ClearLanguageParser::UnitLiteralContext* ctx) {
    const auto lit = std::make_shared<literal>();
    lit->type = type_ref::builtin_type(type{type::kind_enum::unit});
    lit->value = value{lit->type, static_cast<int64_t>(0), false};
    return std::static_pointer_cast<expr>(lit);
}

static std::string unescape_string_token(const std::string& tok) {
    std::string in = tok;
    if (in.size() >= 2 && in.front() == '"' && in.back() == '"') {
        in = in.substr(1, in.size() - 2);
    }
    std::string out; out.reserve(in.size());
    for (size_t i = 0; i < in.size(); ++i) {
        if (in[i] == '\\' && i + 1 < in.size()) {
            switch (const char next = in[i + 1]) {
            case 'b': out.push_back('\b'); break;
            case 'f': out.push_back('\f'); break;
            case 'n': out.push_back('\n'); break;
            case 'r': out.push_back('\r'); break;
            case 't': out.push_back('\t'); break;
            case '\\': out.push_back('\\'); break;
            case '\'': out.push_back('\''); break;
            case '"': out.push_back('"'); break;
            default: out.push_back(next); break;
            }
            ++i;
        }
        else {
            out.push_back(in[i]);
        }
    }
    return out;
}

std::any sema_builder::visitStringLiteral(ClearLanguageParser::StringLiteralContext* ctx) {
	const auto node = std::make_shared<literal>();
	node->type = type_ref::builtin_type(type{ type::kind_enum::string });
	const std::string raw = ctx->STRING()->getText();
    node->value = make_string(unescape_string_token(raw));
	return std::static_pointer_cast<expr>(node);
}

std::any sema_builder::visitBoolLiteral(ClearLanguageParser::BoolLiteralContext* ctx) {
    const auto node = std::make_shared<literal>();
    node->type = boolean_type();
    const bool is_true(ctx->TRUE() != nullptr);
    node->value = value{ node->type, static_cast<int64_t>(is_true ? 1 : 0), false };
    return std::static_pointer_cast<expr>(node);
}
