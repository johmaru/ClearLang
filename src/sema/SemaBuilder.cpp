#include "SemaBuilder.h"
#include "ClearLanguageParser.h"
#include "../core/CLType.h"
#include "../core/SemaUtils.h"
#include "SemaIR.h"
#include <any>
#include <memory>
#include <stdexcept>
#include <string>

using sema::Expr; using sema::Literal; using sema::VarRef;
using sema::Unary; using sema::BinOp; using sema::Block;
using sema::StmtVarDecl; using sema::StmtReturn; using sema::Function;

SemaBuilder::SemaBuilder() : mod_(std::make_shared<sema::Module>()) {
    typeScopes_.emplace_back();
    typeScopes_.back().emplace("i8", TypeRef::builtinType(Type{Type::I8}));
    typeScopes_.back().emplace("u8", TypeRef::builtinType(Type{Type::U8}));
    typeScopes_.back().emplace("i16", TypeRef::builtinType(Type{ Type::I16 }));
	typeScopes_.back().emplace("u16", TypeRef::builtinType(Type{ Type::U16 }));
    typeScopes_.back().emplace("i32", TypeRef::builtinType(Type{Type::I32}));
    typeScopes_.back().emplace("int", TypeRef::builtinType(Type{Type::I32}));
    typeScopes_.back().emplace("u32", TypeRef::builtinType(Type{Type::U32}));
    typeScopes_.back().emplace("i64", TypeRef::builtinType(Type{Type::I64}));
    typeScopes_.back().emplace("u64", TypeRef::builtinType(Type{Type::U64}));
    typeScopes_.back().emplace("f16", TypeRef::builtinType(Type{Type::F16}));
    typeScopes_.back().emplace("noreturn", TypeRef::builtinType(Type{Type::NORETURN}));
    typeScopes_.back().emplace("unit", TypeRef::builtinType(Type{Type::UNIT}));
	typeScopes_.back().emplace("()", TypeRef::builtinType(Type{ Type::UNIT }));
	typeScopes_.back().emplace("string", TypeRef::builtinType(Type{ Type::STRING }));
    varTypes_.emplace_back();

    {
		auto sig = std::make_shared<FunctionSig>();
		sig->paramTypes.push_back(TypeRef::builtinType(Type{ Type::STRING }));
		sig->returnType = std::make_shared<TypeRef>(TypeRef::builtinType(Type{ Type::UNIT }));
		funcSigs_["__cl_printf"] = sig;
    }
}

std::shared_ptr<sema::Module> SemaBuilder::takeModule() { return std::move(mod_); }

TypeRef SemaBuilder::resolveType(const std::string& name) const {
    for (auto it = typeScopes_.rbegin(); it != typeScopes_.rend(); ++it) {
        auto f = it->find(name);
        if (f != it->end()) return f->second;
    }
    return TypeRef::builtinType(Type::fromString(name));
}

TypeRef SemaBuilder::makeTypeRefFrom(ClearLanguageParser::TypeContext* ctx) {
    if (auto nt = dynamic_cast<ClearLanguageParser::NamedTypeContext*>(ctx)) {
        return TypeRef::builtinType(Type::fromString(nt->IDENT()->getText()));
    } else if (dynamic_cast<ClearLanguageParser::UnitTypeContext*>(ctx)) {
        return TypeRef::builtinType(Type{Type::UNIT});
    } else if (auto ft = dynamic_cast<ClearLanguageParser::FunctionTypeContext*>(ctx)) {
        auto sig = std::make_shared<FunctionSig>();
        if (auto tl = ft->typeList()) {
            for (auto* tctx : tl->type()) sig->paramTypes.push_back(makeTypeRefFrom(tctx));
        }
        sig->returnType = std::make_shared<TypeRef>(makeTypeRefFrom(ft->type()));
        return TypeRef::functionType(std::move(sig));
    }
    throw std::runtime_error("unknown type alt");
}

std::any SemaBuilder::visitStart(ClearLanguageParser::StartContext* ctx) {

    for (auto* fd : ctx->funcDecl()) {
        auto name = fd->name->getText();
        auto sig = std::make_shared<FunctionSig>();
        if (auto pl = fd->paramList()) {
            for (auto* p : pl->param()) {
                sig->paramTypes.push_back(makeTypeRefFrom(p->type()));
            }
        }
        sig->returnType = std::make_shared<TypeRef>(makeTypeRefFrom(fd->type()));
        funcSigs_[name] = sig;

        // pick up entry point name if any
        for (auto* at : fd->attributes()) {
            for (auto* id : at->IDENT()) {
                if (id->getText() == "EntryPoint") {
                    mod_->entryName = name;
                }
            }
        }
    }

    for (auto* fd : ctx->funcDecl()) {
        auto fun = std::make_shared<Function>();
        fun->name = fd->name->getText();
        if (auto pl = fd->paramList()) {
            for (auto* p : pl->param()) {
                sema::Param prm;
                prm.name = p->IDENT()->getText();
                prm.type = makeTypeRefFrom(p->type());
                fun->params.push_back(std::move(prm));
            }
        }
        fun->returnType = makeTypeRefFrom(fd->type());
        fun->body = std::make_shared<Block>();

        currentReturnType_ = fun->returnType;
        varTypes_.emplace_back();
        for (auto& prm : fun->params) varTypes_.back()[prm.name] = prm.type;

        auto anyBlk = visit(fd->block());
        *fun->body = *std::any_cast<std::shared_ptr<Block>>(anyBlk);
        varTypes_.pop_back();
        mod_->functions.push_back(std::move(fun));
    }
    return nullptr;
}

std::any SemaBuilder::visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) {
    auto node = std::make_shared<sema::Literal>();
    node->type = TypeRef::builtinType(Type{Type::I32});
    int64_t v = std::stoll(ctx->INT()->getText());
    node->value = Value{node->type, v, true};
    return std::static_pointer_cast<Expr>(node);
}

std::any SemaBuilder::visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) {
    auto node = std::make_shared<Literal>();
    node->type = TypeRef::builtinType(Type{Type::F16});
    float f = std::stof(ctx->FLOAT()->getText());
    sema_utils::checkF16Range(f);
    node->value = Value{node->type, CLHalf{f}, false};
    return std::static_pointer_cast<Expr>(node);
}

std::any SemaBuilder::visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) {
    auto inner = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->inner));

    auto node = std::make_shared<Unary>();
    node->op = "-";
    node->inner = inner;
    node->type = inner->type;
    return std::static_pointer_cast<Expr>(node);
}

std::any SemaBuilder::visitAddExpr(ClearLanguageParser::AddExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->left));
    for (size_t i = 0; i < ctx->right.size(); ++i) {
        auto rhs = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->right[i]));
        std::string op = ctx->op[i]->getText();
        if (!(cur->type.isBuiltin() && rhs->type.isBuiltin() &&
              cur->type.builtin.Kind == rhs->type.builtin.Kind)) {
            throw std::runtime_error("type mismatch in binary op");
        }
        if (is_string(cur->type) && is_string(rhs->type) ) {
	        if (op != "+") {
                throw std::runtime_error("only + operator is supported for string concatenation");
			}
        }
        auto bin = std::make_shared<BinOp>();
        bin->op = op;
        bin->lhs = cur;
        bin->rhs = rhs;
        bin->type = cur->type;
        cur = bin;
    }
    return cur;
}

std::any SemaBuilder::visitMulExpr(ClearLanguageParser::MulExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->left));
    for (size_t i = 0; i < ctx->right.size(); ++i) {
        auto rhs = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->right[i]));
        std::string op = ctx->op[i]->getText();
        if (!(cur->type.isBuiltin() && rhs->type.isBuiltin() &&
              cur->type.builtin.Kind == rhs->type.builtin.Kind)) {
            throw std::runtime_error("type mismatch in binary op");
        }
        auto bin = std::make_shared<BinOp>();
        bin->op = op;
        bin->lhs = cur;
        bin->rhs = rhs;
        bin->type = cur->type;
        cur = bin;
    }
    return cur;
}

std::any SemaBuilder::visitVarRef(ClearLanguageParser::VarRefContext* ctx) {
    auto node = std::make_shared<VarRef>();
    node->name = ctx->IDENT()->getText();
    for (auto it = varTypes_.rbegin(); it != varTypes_.rend(); ++it) {
        auto f = it->find(node->name);
        if (f != it->end()) { node->type = f->second; return std::static_pointer_cast<Expr>(node); }
    }
    throw std::runtime_error("undefined variable: " + node->name);
}

std::any SemaBuilder::visitBlock(ClearLanguageParser::BlockContext* ctx) {
    auto blk = std::make_shared<Block>();
    for (auto* s : ctx->stmt()) {
        if (auto* sr = dynamic_cast<ClearLanguageParser::StmtReturnContext*>(s)) {
            blk->statements.push_back(std::any_cast<std::shared_ptr<StmtReturn>>(visit(sr)));
        } else if (auto* vd = dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(s)) {
            blk->statements.push_back(std::any_cast<std::shared_ptr<StmtVarDecl>>(visit(vd)));
        }
        else if (auto* se = dynamic_cast<ClearLanguageParser::StmtExprContext*>(s)) {
			blk->statements.push_back(std::any_cast<std::shared_ptr<sema::StmtExpr>>(visit(se)));
        }
    	else {
            // currently pass expression statements
            visit(s);
        }
    }
    return blk;
}

std::any SemaBuilder::visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) {
    auto* vd = ctx->varDecl();
    auto node = std::make_shared<StmtVarDecl>();
    node->name = vd->IDENT()->getText();
    node->declType = makeTypeRefFrom(vd->type());

    std::shared_ptr<Expr> initExpr;
    if (vd->expr()) {
        initExpr = std::any_cast<std::shared_ptr<Expr>>(visit(vd->expr()));
        if (initExpr->type.isBuiltin() && initExpr->type.builtin.Kind == Type::I32) {
            if (auto lit = std::dynamic_pointer_cast<Literal>(initExpr)) {
                if (lit->value.isUntypedInt) {
                    auto coerced = sema_utils::coerceUntypedIntTo(lit->value, node->declType);
                    lit->value = coerced;
                    lit->type = node->declType;
                    initExpr->type = node->declType;
                }
            }
        }
        if (initExpr->type.isBuiltin() &&
            initExpr->type.builtin.Kind != node->declType.builtin.Kind) {
            throw std::runtime_error("type mismatch in var init");
        }
    }
    node->initExpr = initExpr;
    
    varTypes_.back().emplace(node->name, node->declType);
    return node;
}

std::any SemaBuilder::visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) {
    auto node = std::make_shared<StmtReturn>();
    if (ctx->expr()) {
        node->value = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->expr()));
    }
    return node;
}

std::any SemaBuilder::visitStmtExpr(ClearLanguageParser::StmtExprContext* ctx)
{
	auto node = std::make_shared<sema::StmtExpr>();
	node->expr = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->expr()));
	return node;
}


std::any SemaBuilder::visitUnaryPrimary(ClearLanguageParser::UnaryPrimaryContext* ctx) {
    // primary -> int/float/ident/paren/unit
    return visit(ctx->postfixExpr());
}

std::any SemaBuilder::visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) {
    if (!ctx->callSuffix().empty()) {
        auto* prim = ctx->primary();
        auto* vctx = dynamic_cast<ClearLanguageParser::VarRefContext*>(prim);
        if (!vctx) throw std::runtime_error("call on non-ident not supported");
        auto call = std::make_shared<sema::Call>();
        call->callee = vctx->IDENT()->getText();

        for (auto* cs : ctx->callSuffix()) {
            call->args.clear();
            if (auto al = cs->argList()) {
                for (auto* ectx : al->expr()) {
                    call->args.push_back(std::any_cast<std::shared_ptr<Expr>>(visit(ectx)));
                }
            }
            auto it = funcSigs_.find(call->callee);
            if (it == funcSigs_.end()) throw std::runtime_error("unknown function: " + call->callee);
            auto sig = it->second;
            if (sig->paramTypes.size() != call->args.size()) {
                throw std::runtime_error("arg count mismatch for function: " + call->callee);
            }
            for (size_t i = 0; i < call->args.size(); ++i) {
                if (!(call->args[i]->type.isBuiltin() && sig->paramTypes[i].isBuiltin() &&
                      call->args[i]->type.builtin.Kind == sig->paramTypes[i].builtin.Kind)) {
                    throw std::runtime_error("arg type mismatch for function: " + call->callee);
                }
            }
            call->type = *sig->returnType;
        }
        return std::static_pointer_cast<Expr>(call);
    }
    return visit(ctx->primary());
}

std::any SemaBuilder::visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) {
    return visit(ctx->expr());
}

std::any SemaBuilder::visitUnitLiteral(ClearLanguageParser::UnitLiteralContext* ctx) {
    auto lit = std::make_shared<Literal>();
    lit->type = TypeRef::builtinType(Type{Type::UNIT});
    lit->value = Value{lit->type, (int64_t)0, false};
    return std::static_pointer_cast<Expr>(lit);
}

static std::string unescapeStringToken(const std::string& tok) {
    std::string in = tok;
    if (in.size() >= 2 && in.front() == '"' && in.back() == '"') {
        in = in.substr(1, in.size() - 2);
    }
    std::string out; out.reserve(in.size());
    for (size_t i = 0; i < in.size(); ++i) {
        if (in[i] == '\\' && i + 1 < in.size()) {
            switch (char next = in[i + 1]) {
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

std::any SemaBuilder::visitStringLiteral(ClearLanguageParser::StringLiteralContext* context) {
	auto node = std::make_shared<Literal>();
	node->type = TypeRef::builtinType(Type{ Type::STRING });
	std::string raw = context->STRING()->getText();
    node->value = make_string(unescapeStringToken(raw));
	return std::static_pointer_cast<Expr>(node);
}
