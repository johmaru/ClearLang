#include "SemaBuilder.h"

#include "../core/CLType.h"
#include "../core/SemaUtils.h"
#include "ClearLanguageParser.h"
#include "SemaIR.h"

#include <any>
#include <cmath>
#include <llvm/ADT/APFloat.h>
#include <memory>
#include <stdexcept>
#include <string>

using sema::BinOp;
using sema::Block;
using sema::Cast;
using sema::Expr;
using sema::Function;
using sema::Literal;
using sema::StmtExpr;
using sema::StmtIf;
using sema::StmtReturn;
using sema::StmtVarDecl;
using sema::Unary;
using sema::VarRef;

using sema::Call;
using sema::Cast;

namespace {
bool isIntKind(const Type::kind_enum KIND) {
    switch (KIND) {
    case Type::kind_enum::I8:
    case Type::kind_enum::U8:
    case Type::kind_enum::I16:
    case Type::kind_enum::U16:
    case Type::kind_enum::I32:
    case Type::kind_enum::U32:
    case Type::kind_enum::I64:
    case Type::kind_enum::U64:
        return true;
    default:
        return false;
    }
}

bool isBoolKind(const Type::kind_enum KIND) {
    switch (KIND) {
    case Type::kind_enum::BOOLEAN:
        return true;

    default:
        return false;
    }
}

bool isNumKind(const Type::kind_enum KIND) {
    switch (KIND) {
    case Type::kind_enum::I8:
    case Type::kind_enum::U8:
    case Type::kind_enum::I16:
    case Type::kind_enum::U16:
    case Type::kind_enum::I32:
    case Type::kind_enum::U32:
    case Type::kind_enum::I64:
    case Type::kind_enum::U64:
    case Type::kind_enum::F16:
    case Type::kind_enum::F32:
        return true;
    default:
        return false;
    }
}

TypeRef booleanType() {
    return TypeRef::builtinType(Type{Type::kind_enum::BOOLEAN});
}
} // namespace

SemaBuilder::SemaBuilder() : mod_(std::make_shared<sema::Module>()) {
    pushScope(scope_kind::GLOBAL);
    registerBuiltinTypes();

    auto add_builtin_function = [&](const char* name, std::initializer_list<Type::kind_enum> params,
                                    Type::kind_enum ret) {
        auto sig = std::make_shared<FunctionSig>();
        for (auto kind : params) {
            sig->param_types.push_back(TypeRef::builtinType(Type{kind}));
        }
        sig->return_type = std::make_shared<TypeRef>(TypeRef::builtinType(Type{ret}));

        SymbolEntry sym_entry;
        sym_entry.kind = symbol_kind::FUNCTION;
        sym_entry.function_sig = sig;
        sym_entry.type = TypeRef::functionType(sig);
        insertSymbol(name, sym_entry);
    };

    // Signatures of built-in functions

    add_builtin_function("_cl_printf", {Type::kind_enum::STRING}, Type::kind_enum::UNIT);

    add_builtin_function("__cl_i8_printf", {Type::kind_enum::I8}, Type::kind_enum::UNIT);

    add_builtin_function("__cl_i8_printfn", {Type::kind_enum::I8}, Type::kind_enum::UNIT);

    add_builtin_function("__cl_u8_printfn", {Type::kind_enum::U8}, Type::kind_enum::UNIT);

    add_builtin_function("__cl_i16_printfn", {Type::kind_enum::I16}, Type::kind_enum::UNIT);

    add_builtin_function("__cl_f16_printfn", {Type::kind_enum::F16}, Type::kind_enum::UNIT);

    add_builtin_function("__cl_f32_printfn", {Type::kind_enum::F32}, Type::kind_enum::UNIT);

    {
        auto add_parse = [&](const char* name, Type::kind_enum kind) {
            const auto SIG = std::make_shared<FunctionSig>();
            SIG->param_types.push_back(TypeRef::builtinType(Type{Type::kind_enum::STRING}));
            SIG->return_type = std::make_shared<TypeRef>(TypeRef::builtinType(Type{kind}));
            SymbolEntry sym_entry;
            sym_entry.kind = symbol_kind::FUNCTION;
            sym_entry.function_sig = SIG;
            sym_entry.type = TypeRef::functionType(SIG);
            insertSymbol(name, sym_entry);
        };
        add_parse("__cl_parse_i8", Type::kind_enum::I8);
        add_parse("__cl_parse_u8", Type::kind_enum::U8);
        add_parse("__cl_parse_i16", Type::kind_enum::I16);
        add_parse("__cl_parse_u16", Type::kind_enum::U16);
        add_parse("__cl_parse_i32", Type::kind_enum::I32);
        add_parse("__cl_parse_u32", Type::kind_enum::U32);
        add_parse("__cl_parse_i64", Type::kind_enum::I64);
        add_parse("__cl_parse_u64", Type::kind_enum::U64);
    }

    // builtin functions for build.clr
    add_builtin_function("__set_entry", {Type::kind_enum::STRING}, Type::kind_enum::UNIT);
    add_builtin_function("__add_source", {Type::kind_enum::STRING}, Type::kind_enum::UNIT);
    add_builtin_function("__set_output", {Type::kind_enum::STRING}, Type::kind_enum::UNIT);
    add_builtin_function("__set_target", {Type::kind_enum::STRING}, Type::kind_enum::UNIT);
    add_builtin_function("__set_app_name", {Type::kind_enum::STRING}, Type::kind_enum::UNIT);
}

bool SemaBuilder::tryFoldExpr(const std::shared_ptr<sema::Expr>& exprPtr,
                              value_variant& out) const {
    using kind = Type::kind_enum;
    if (auto lit = std::dynamic_pointer_cast<sema::Literal>(exprPtr)) {
        if (lit->type.isBuiltin()) {
            switch (lit->type.builtin.kind) {
            case kind::I8:
            case kind::I16:
            case kind::I32:
            case kind::I64: {
                if (std::holds_alternative<int64_t>(lit->value.v)) {
                    out = static_cast<int64_t>(std::get<int64_t>(lit->value.v));
                    return true;
                }
                if (std::holds_alternative<uint64_t>(lit->value.v)) {
                    out = static_cast<uint64_t>(std::get<uint64_t>(lit->value.v));
                    return true;
                }
                break;
            }
            case kind::U8:
            case kind::U16:
            case kind::U32:
            case kind::U64: {
                if (std::holds_alternative<uint64_t>(lit->value.v)) {
                    out = std::get<uint64_t>(lit->value.v);
                    return true;
                }
                if (std::holds_alternative<int64_t>(lit->value.v)) {
                    out = static_cast<uint64_t>(std::get<int64_t>(lit->value.v));
                    return true;
                }
                break;
            }
            case kind::F16:
            case kind::F32: {
                if (std::holds_alternative<ClF32>(lit->value.v)) {
                    ClF32 val = std::get<ClF32>(lit->value.v);
                    float float_val = NAN;
                    static_assert(sizeof(float) == 4, "expected IEEE single");
                    uint32_t bits = val.bits;
                    std::memcpy(&float_val, &bits, sizeof(float));
                    out = static_cast<double>(float_val);
                    return true;
                }
                break;
            }
            case kind::BOOLEAN: {
                if (std::holds_alternative<int64_t>(lit->value.v)) {
                    out = (std::get<int64_t>(lit->value.v) != 0);
                    return true;
                }
                break;
            }
            case kind::STRING:
            case kind::UNIT:
            case kind::NORETURN:
                break;
            }
        }
        return false;
    }
    if (auto un_v = std::dynamic_pointer_cast<sema::Unary>(exprPtr)) {
        value_variant inner;
        if (!tryFoldExpr(un_v->inner, inner)) {
            return false;
        }
        if (un_v->op == "-") {
            if (auto* pi_v = std::get_if<int64_t>(&inner)) {
                out = -(*pi_v);
                return true;
            }
            if (auto* pu_v = std::get_if<uint64_t>(&inner)) {
                out = (-static_cast<int64_t>(*pu_v));
                return true;
            }
            if (auto* pd_v = std::get_if<double>(&inner)) {
                out = -(*pd_v);
                return true;
            }
        }
        return false;
    }
    if (auto bin = std::dynamic_pointer_cast<sema::BinOp>(exprPtr)) {
        value_variant lv_v;
        value_variant rv_v;
        if (!tryFoldExpr(bin->lhs, lv_v) || !tryFoldExpr(bin->rhs, rv_v)) {
            return false;
        }
        auto bin_op = bin->op;
        auto getd = [](const value_variant& v_v) -> double {
            if (const auto* const PD_V = std::get_if<double>(&v_v)) {
                return *PD_V;
            }
            if (const auto* const PI_V = std::get_if<int64_t>(&v_v)) {
                return static_cast<double>(*PI_V);
            }
            if (const auto* const PU_V = std::get_if<uint64_t>(&v_v)) {
                return static_cast<double>(*PU_V);
            }
            return 0.0;
        };
        auto geti = [](const value_variant& v_v) -> int64_t {
            if (const auto* const PI_V = std::get_if<int64_t>(&v_v)) {
                return *PI_V;
            }
            if (const auto* const PU_V = std::get_if<uint64_t>(&v_v)) {
                return static_cast<int64_t>(*PU_V);
            }
            return 0;
        };

        bool use_double =
            std::holds_alternative<double>(lv_v) || std::holds_alternative<double>(rv_v);
        if (use_double) {
            double lhs = getd(lv_v);
            double rhs = getd(rv_v);
            if (bin_op == "+") {
                out = lhs + rhs;
            } else if (bin_op == "-") {
                out = lhs - rhs;
            } else if (bin_op == "*") {
                out = lhs * rhs;
            } else if (bin_op == "/") {
                out = lhs / rhs;
            } else {
                return false;
            }
            return true;
        }
        int64_t lhs = geti(lv_v);
        int64_t rhs = geti(rv_v);
        if (bin_op == "+") {
            out = (lhs + rhs);
        } else if (bin_op == "-") {
            out = (lhs - rhs);
        } else if (bin_op == "*") {
            out = (lhs * rhs);
        } else if (bin_op == "/") {
            if (rhs == 0) {
                return false;
            }
            out = (lhs / rhs);
        } else if (bin_op == "%") {
            if (rhs == 0) {
                return false;
            }
            out = (lhs % rhs);
        } else {
            return false;
        }
        return true;
    }
    return false;
}

void SemaBuilder::registerBuiltinTypes() {

    static constexpr std::array<std::pair<const char*, Type::kind_enum>, 16> BUILTINS = {
        {{"i8", Type::kind_enum::I8},
         {"u8", Type::kind_enum::U8},
         {"i16", Type::kind_enum::I16},
         {"u16", Type::kind_enum::U16},
         {"i32", Type::kind_enum::I32},
         {"int", Type::kind_enum::I32},
         {"u32", Type::kind_enum::U32},
         {"i64", Type::kind_enum::I64},
         {"u64", Type::kind_enum::U64},
         {"f16", Type::kind_enum::F16},
         {"f32", Type::kind_enum::F32},
         {"noreturn", Type::kind_enum::NORETURN},
         {"unit", Type::kind_enum::UNIT},
         {"()", Type::kind_enum::UNIT},
         {"string", Type::kind_enum::STRING},
         {"bool", Type::kind_enum::BOOLEAN}}};
    for (const auto& built : BUILTINS) {
        SymbolEntry entry;
        entry.kind = symbol_kind::TYPE_NAME;
        entry.type = TypeRef::builtinType(Type{built.second});
        insertSymbol(built.first, entry);
    }
}

std::shared_ptr<sema::Module> SemaBuilder::takeModule() {
    return std::move(mod_);
}

std::string SemaBuilder::resolveFunctionName(const std::string& name) const {
    if (name.find(DOUBLE_COLON) != std::string::npos) {
        if (const auto* sym_entry = lookupSymbol(name);
            (sym_entry != nullptr) && sym_entry->kind == symbol_kind::FUNCTION) {
            return name;
        }

        throw std::runtime_error("undefined function (qualified): " + name);
    }

    if (const auto* sym_entry = lookupSymbol(name);
        (sym_entry != nullptr) && sym_entry->kind == symbol_kind::FUNCTION) {
        return name;
    }

    if (!current_package_.empty()) {
        std::string qualified_name = qualify(name);
        if (const auto* sym_entry = lookupSymbol(qualified_name);
            (sym_entry != nullptr) && sym_entry->kind == symbol_kind::FUNCTION) {
            return qualified_name;
        }
    }

    throw std::runtime_error("undefined function: " + name);
}

const SemaBuilder::SymbolEntry* SemaBuilder::lookupFunctionSymbol(const std::string& name) const {
    if (const auto* sym_entry = lookupSymbol(name);
        (sym_entry != nullptr) && sym_entry->kind == symbol_kind::FUNCTION) {
        return sym_entry;
    }

    if (!current_package_.empty()) {
        std::string qualified_package_name = current_package_ + DOUBLE_COLON + name;
        if (const auto* sym_entry = lookupSymbol(qualified_package_name);
            (sym_entry != nullptr) && sym_entry->kind == symbol_kind::FUNCTION) {
            return sym_entry;
        }
    }
    for (const auto& k_v : imports_) {
        std::string cand = k_v.second + DOUBLE_COLON + name;
        if (const auto* sym_entry = lookupSymbol(cand);
            (sym_entry != nullptr) && sym_entry->kind == symbol_kind::FUNCTION) {
            return sym_entry;
        }

        std::string alias_cand = k_v.first + DOUBLE_COLON + name;
        if (const auto* sym_entry = lookupSymbol(alias_cand);
            (sym_entry != nullptr) && sym_entry->kind == symbol_kind::FUNCTION) {
            return sym_entry;
        }
    }
    return nullptr;
}

bool SemaBuilder::insertSymbol(const std::string& name, SymbolEntry& entry) {
    if (symbol_scopes_.empty()) {
        pushScope(scope_kind::GLOBAL);
    }
    auto& cur = symbol_scopes_.back().symbols;
    if (cur.count(name) != 0U) {
        return false;
    }
    cur.emplace(name, entry);
    return true;
}

TypeRef SemaBuilder::resolveTypeSymbol(const std::string& name) const {
    if (const auto* sem_entry = lookupSymbol(name)) {
        if (sem_entry->kind == symbol_kind::TYPE_NAME) {
            return sem_entry->type;
        }
    }
    throw std::runtime_error("unknown type: " + name);
}

TypeRef SemaBuilder::resolveType(const std::string& name) const {
    if (const auto* sem_entry = lookupSymbol(name);
        (sem_entry != nullptr) && sem_entry->kind == symbol_kind::TYPE_NAME) {
        return sem_entry->type;
    }

    if (!current_package_.empty()) {
        std::string qualified_package_name = current_package_ + DOUBLE_COLON + name;
        if (const auto* sem_entry = lookupSymbol(qualified_package_name);
            (sem_entry != nullptr) && sem_entry->kind == symbol_kind::TYPE_NAME) {
            return sem_entry->type;
        }
    }

    for (const auto& k_v : imports_) {
        std::string cand = k_v.second + DOUBLE_COLON + name;
        if (const auto* sym_entry = lookupSymbol(cand);
            (sym_entry != nullptr) && sym_entry->kind == symbol_kind::TYPE_NAME) {
            return sym_entry->type;
        }
        std::string alias_cand = k_v.first + DOUBLE_COLON + name;
        if (const auto* sym_entry = lookupSymbol(alias_cand);
            (sym_entry != nullptr) && sym_entry->kind == symbol_kind::TYPE_NAME) {
            return sym_entry->type;
        }
    }

    try {
        return TypeRef::builtinType(Type::fromString(name));
    } // NOLINTNEXTLINE(bugprone-empty-catch)
    catch (...) {
    }

    throw std::runtime_error("unknown type: " + name);
}

TypeRef SemaBuilder::makeTypeRefFrom(ClearLanguageParser::TypeContext* ctx) {
    if (auto* const NAME_TYPE_CTX = dynamic_cast<ClearLanguageParser::NamedTypeContext*>(ctx)) {
        return resolveType(NAME_TYPE_CTX->IDENT()->getText());
    }
    if (dynamic_cast<ClearLanguageParser::UnitTypeContext*>(ctx) != nullptr) {
        return TypeRef::builtinType(Type{Type::kind_enum::UNIT});
    }
    if (auto* const FUNCTION_TYPE_CTX =
            dynamic_cast<ClearLanguageParser::FunctionTypeContext*>(ctx)) {
        auto sig = std::make_shared<FunctionSig>();
        if (auto* const TYPE_LIST_CTX = FUNCTION_TYPE_CTX->typeList()) {
            for (auto* tctx : TYPE_LIST_CTX->type()) {
                sig->param_types.push_back(makeTypeRefFrom(tctx));
            }
        }
        sig->return_type = std::make_shared<TypeRef>(makeTypeRefFrom(FUNCTION_TYPE_CTX->type()));
        return TypeRef::functionType(std::move(sig));
    }
    throw std::runtime_error("unknown type alt");
}

// Collect function signatures and constant declarations
void SemaBuilder::collectSignatures(ClearLanguageParser::StartContext* ctx) {
    FileCtxGuard guard(*this, ctx);

    for (auto* const_decl : ctx->constantDecl()) {
        visit(const_decl);
    }

    for (auto* func_decl_ctx : ctx->funcDecl()) {
        std::string qualified_name = qualify(func_decl_ctx->name->getText());

        const auto SIG = std::make_shared<FunctionSig>();
        if (auto* const PARAM_LIST_CTX = func_decl_ctx->paramList()) {
            for (auto* param_ctx : PARAM_LIST_CTX->param()) {
                SIG->param_types.push_back(makeTypeRefFrom(param_ctx->type()));
            }
        }
        SIG->return_type = std::make_shared<TypeRef>(makeTypeRefFrom(func_decl_ctx->type()));

        SymbolEntry sym_entry;
        sym_entry.kind = symbol_kind::FUNCTION;
        sym_entry.function_sig = SIG;
        sym_entry.type = TypeRef::functionType(SIG);
        if (!insertSymbol(qualified_name, sym_entry)) {
            throw std::runtime_error("function redeclaration: " + qualified_name);
        }

        // pick up entry point name if any
        for (auto* at_ctx : func_decl_ctx->attributes()) {
            for (auto* term_node : at_ctx->IDENT()) {
                if (term_node->getText() == "EntryPoint") {
                    mod_->entry_name = qualified_name;
                }
            }
        }
    }
}

// Construct function bodies
void SemaBuilder::constructTarget(ClearLanguageParser::StartContext* ctx) {
    FileCtxGuard guard(*this, ctx);

    for (auto* func_decl_ctx : ctx->funcDecl()) {
        auto func = std::make_shared<Function>();
        func->name = qualify(func_decl_ctx->name->getText());

        if (auto* const PARAM_LIST_CTX = func_decl_ctx->paramList()) {
            for (auto* param_ctx : PARAM_LIST_CTX->param()) {
                sema::Param prm;
                prm.name = param_ctx->IDENT()->getText();
                prm.type = makeTypeRefFrom(param_ctx->type());
                func->params.push_back(std::move(prm));
            }
        }
        func->return_type = makeTypeRefFrom(func_decl_ctx->type());
        func->body = std::make_shared<Block>();

        current_return_type_ = func->return_type;

        ScopeGuard guard(*this, scope_kind::FUNCTION);

        for (auto& prm : func->params) {
            SymbolEntry sym_entry;
            sym_entry.kind = symbol_kind::VARIABLE;
            sym_entry.type = prm.type;
            sym_entry.mut = sema::mutability::VAR;
            if (!insertSymbol(prm.name, sym_entry)) {
                throw std::runtime_error("parameter redeclaration: " + prm.name);
            }
        }

        auto any_blk = visit(func_decl_ctx->block());
        *func->body = *std::any_cast<std::shared_ptr<Block>>(any_blk);

        mod_->functions.push_back(std::move(func));
    }
}

std::any SemaBuilder::visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) {
    const auto NODE = std::make_shared<sema::Literal>();
    NODE->type = TypeRef::builtinType(Type{Type::kind_enum::I32});
    int64_t int_value = std::stoll(ctx->INT()->getText());
    NODE->value = Value{NODE->type, int_value, true};
    return std::static_pointer_cast<Expr>(NODE);
}

std::any SemaBuilder::visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) {
    const auto NODE = std::make_shared<Literal>();
    NODE->type = TypeRef::builtinType(Type{Type::kind_enum::F32});
    const std::string TOK = ctx->FLOAT()->getText();
    const llvm::APFloat APF(llvm::APFloat::IEEEsingle(), TOK);
    if (APF.isInfinity()) {
        throw std::runtime_error("f32: Out of range" + TOK);
    }
    const uint32_t BITS = static_cast<uint32_t>(APF.bitcastToAPInt().getZExtValue());
    ClF32 h_v;
    h_v.bits = BITS;

    NODE->value = Value{NODE->type, h_v, false};
    return std::static_pointer_cast<Expr>(NODE);
}

std::any SemaBuilder::visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) {
    const auto INNER = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->inner));

    const auto NODE = std::make_shared<Unary>();
    NODE->op = "-";
    NODE->inner = INNER;
    NODE->type = INNER->type;
    return std::static_pointer_cast<Expr>(NODE);
}

std::any SemaBuilder::visitOrExpr(ClearLanguageParser::OrExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->left));

    if (ctx->right.empty()) {
        return cur;
    }

    if (!(cur->type.isBuiltin() && isIntKind(cur->type.builtin.kind))) {
        throw std::runtime_error("operator 'or' expects integer operands (left)");
    }

    for (auto& aec : ctx->right) {
        const auto RHS = std::any_cast<std::shared_ptr<Expr>>(visit(aec));
        if (!(RHS->type.isBuiltin() && isIntKind(RHS->type.builtin.kind))) {
            throw std::runtime_error("operator or expects integer operands (right)");
        }

        const auto NODE = std::make_shared<BinOp>();
        NODE->op = "or";
        NODE->lhs = cur;
        NODE->rhs = RHS;
        NODE->type = booleanType();
        cur = NODE;
    }
    return cur;
}

std::any SemaBuilder::visitAndExpr(ClearLanguageParser::AndExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->left));

    if (ctx->right.empty()) {
        return cur;
    }

    if (!(cur->type.isBuiltin() && isIntKind(cur->type.builtin.kind))) {
        throw std::runtime_error("operator 'and' expects integer operands (left)");
    }

    for (auto& eec : ctx->right) {
        const auto RHS = std::any_cast<std::shared_ptr<Expr>>(visit(eec));
        if (!(RHS->type.isBuiltin() && isIntKind(RHS->type.builtin.kind))) {
            throw std::runtime_error("operator 'and' expects integer operands (right)");
        }

        const auto NODE = std::make_shared<BinOp>();
        NODE->op = "and";
        NODE->lhs = cur;
        NODE->rhs = RHS;
        NODE->type = booleanType();
        cur = NODE;
    }
    return cur;
}

std::any SemaBuilder::visitEqualExpr(ClearLanguageParser::EqualExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->left));

    for (size_t i = 0; i < ctx->right.size(); i++) {
        const auto RHS = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->right[i]));
        const std::string OPER = ctx->op[i]->getText();
        if (!(cur->type.isBuiltin() && RHS->type.isBuiltin())) {
            throw std::runtime_error("equal operator requires builtin types");
        }
        const auto L_K = cur->type.builtin.kind;
        const auto R_K = RHS->type.builtin.kind;

        if (!isNumKind(L_K) || L_K != R_K) {
            throw std::runtime_error("type mismatch in equal op (only num same types)");
        }

        const auto NODE = std::make_shared<BinOp>();
        NODE->op = OPER;
        NODE->lhs = cur;
        NODE->rhs = RHS;
        NODE->type = booleanType();
        cur = NODE;
    }
    return cur;
}

std::any SemaBuilder::visitAddExpr(ClearLanguageParser::AddExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->left));
    for (size_t i = 0; i < ctx->right.size(); ++i) {
        const auto RHS = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->right[i]));
        std::string oper = ctx->op[i]->getText();
        if (!cur->type.isBuiltin() || !RHS->type.isBuiltin() ||
            cur->type.builtin.kind != RHS->type.builtin.kind) {
            throw std::runtime_error("type mismatch in binary op");
        }
        if (isString(cur->type) && isString(RHS->type)) {
            if (oper != "+") {
                throw std::runtime_error("only + operator is supported for string concatenation");
            }
        }
        const auto BIN = std::make_shared<BinOp>();
        BIN->op = oper;
        BIN->lhs = cur;
        BIN->rhs = RHS;
        BIN->type = cur->type;
        cur = BIN;
    }
    return cur;
}

std::any SemaBuilder::visitMulExpr(ClearLanguageParser::MulExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->left));
    for (size_t i = 0; i < ctx->right.size(); ++i) {
        const auto RHS = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->right[i]));
        const std::string OPER = ctx->op[i]->getText();
        if (!cur->type.isBuiltin() || !RHS->type.isBuiltin() ||
            cur->type.builtin.kind != RHS->type.builtin.kind) {
            throw std::runtime_error("type mismatch in binary op");
        }

        if ((cur->type.isBuiltin() && !isNumKind(cur->type.builtin.kind)) ||
            (RHS->type.isBuiltin() && !isNumKind(RHS->type.builtin.kind))) {
            throw std::runtime_error("operator " + OPER + " requires numeric operand types");
        }

        const auto BIN = std::make_shared<BinOp>();
        BIN->op = OPER;
        BIN->lhs = cur;
        BIN->rhs = RHS;
        BIN->type = cur->type;
        cur = BIN;
    }
    return cur;
}

std::any SemaBuilder::visitUnaryPrimary(ClearLanguageParser::UnaryPrimaryContext* ctx) {
    return visit(ctx->postfixExpr());
}

std::any SemaBuilder::visitVarStmtDecl(ClearLanguageParser::VarStmtDeclContext* ctx) {
    return visitChildren(ctx);
}

std::any SemaBuilder::visitParenConstExpr(ClearLanguageParser::ParenConstExprContext* ctx) {
    return visit(ctx->constExpr());
}

std::any SemaBuilder::visitVarRef(ClearLanguageParser::VarRefContext* ctx) {
    std::string name;
    if (auto* qual_ident_ctx = ctx->qualifiedIdent()) {
        name = qual_ident_ctx->getText();
    } else {
        name = ctx->getText();
    }

    if (const auto* sym_entry = lookupSymbol(name)) {
        switch (sym_entry->kind) {
        case symbol_kind::VARIABLE: {
            auto v_ref = std::make_shared<VarRef>();
            v_ref->name = name;
            v_ref->type = sym_entry->type;
            return std::static_pointer_cast<Expr>(v_ref);
        }
        case symbol_kind::FUNCTION: {
            // fallback
            break;
        }
        case symbol_kind::CONSTANT: {
            if (!std::holds_alternative<std::monostate>(sym_entry->value)) {
                const auto LIT = std::make_shared<Literal>();
                LIT->type = sym_entry->type;

                if (const auto* const PI_V = std::get_if<int64_t>(&sym_entry->value)) {
                    LIT->value = Value{LIT->type, *PI_V, false};
                } else if (const auto* const PU_V = std::get_if<uint64_t>(&sym_entry->value)) {
                    LIT->value = Value{LIT->type, *PU_V, false};
                } else if (const auto* const PB_V = std::get_if<bool>(&sym_entry->value)) {
                    LIT->value = Value{LIT->type, static_cast<int64_t>(*PB_V ? 1 : 0), false};
                } else if (const auto* const PD_V = std::get_if<double>(&sym_entry->value)) {
                    const auto FLOAT_VAL = static_cast<float>(*PD_V);
                    ClF32 bits;
                    std::memcpy(&bits, &FLOAT_VAL, sizeof(float));
                    LIT->value = Value{LIT->type, bits, false};
                } else if (const auto* const PS_V = std::get_if<std::string>(&sym_entry->value)) {
                    LIT->value = makeString(*PS_V);
                }
                return std::static_pointer_cast<Expr>(LIT);
            }
            return sym_entry->const_expr;
        }
        case symbol_kind::TYPE_NAME:
            throw std::runtime_error("type name used as value: " + name);
        }
    }

    try {
        const std::string FQ_NAME = resolveFunctionName(name);
        const auto* fse = lookupSymbol(FQ_NAME);

        if ((fse == nullptr) || fse->kind != symbol_kind::FUNCTION) {
            throw std::runtime_error("function symbol disappeared: " + FQ_NAME);
        }

        const auto V_REF = std::make_shared<VarRef>();
        V_REF->name = FQ_NAME;
        V_REF->type = fse->type;
        return std::static_pointer_cast<Expr>(V_REF);
    }
    // NOLINTNEXTLINE(bugprone-empty-catch)
    catch (...) {
    }
    throw std::runtime_error("undefined identifier: " + name);
}

std::any SemaBuilder::visitBlock(ClearLanguageParser::BlockContext* ctx) {
    auto blk = std::make_shared<Block>();
    ScopeGuard guard(*this, scope_kind::BLOCK);

    for (auto* stmt_ctx : ctx->stmt()) {
        if (auto* stmt_return_ctx =
                dynamic_cast<ClearLanguageParser::StmtReturnContext*>(stmt_ctx)) {
            blk->statements.push_back(
                std::any_cast<std::shared_ptr<StmtReturn>>(visit(stmt_return_ctx)));
        } else if (auto* stmt_var_decl_ctx =
                       dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(stmt_ctx)) {
            blk->statements.push_back(
                std::any_cast<std::shared_ptr<StmtVarDecl>>(visit(stmt_var_decl_ctx)));
        } else if (auto* stmt_expr_ctx =
                       dynamic_cast<ClearLanguageParser::StmtExprContext*>(stmt_ctx)) {
            blk->statements.push_back(
                std::any_cast<std::shared_ptr<StmtExpr>>(visit(stmt_expr_ctx)));
        } else if (auto* stmt_if_ctx =
                       dynamic_cast<ClearLanguageParser::StmtIfContext*>(stmt_ctx)) {
            blk->statements.push_back(std::any_cast<std::shared_ptr<StmtIf>>(visit(stmt_if_ctx)));
        } else {
            // currently pass expression statements
            visit(stmt_ctx);
        }
    }
    return blk;
}

std::any SemaBuilder::visitIfBlock(ClearLanguageParser::IfBlockContext* ctx) {
    const auto COND = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->expr()));

    const bool IS_BOOL = COND->type.isBuiltin() && isBoolKind(COND->type.builtin.kind);
    const bool IS_INT = COND->type.isBuiltin() && isIntKind(COND->type.builtin.kind);

    if (!(IS_BOOL || IS_INT)) {
        throw std::runtime_error("if condition must be boolean or int expression");
    }

    auto node = std::make_shared<StmtIf>();
    node->cond = COND;
    node->then_blk = std::any_cast<std::shared_ptr<Block>>(visit(ctx->block(0)));

    node->else_blk = nullptr;
    const auto BLKS = ctx->block();
    auto* else_stmt_ctx = ctx->stmt();
    if (BLKS.size() >= 2) {
        node->else_blk = std::any_cast<std::shared_ptr<Block>>(visit(ctx->block(1)));
    } else if (else_stmt_ctx != nullptr) {
        const auto ELSE_BLK = std::make_shared<Block>();
        if (auto* stmt_return_ctx =
                dynamic_cast<ClearLanguageParser::StmtReturnContext*>(else_stmt_ctx)) {
            ELSE_BLK->statements.push_back(
                std::any_cast<std::shared_ptr<StmtReturn>>(visit(stmt_return_ctx)));
        } else if (auto* var_decl_ctx =
                       dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(else_stmt_ctx)) {
            ELSE_BLK->statements.push_back(
                std::any_cast<std::shared_ptr<StmtVarDecl>>(visit(var_decl_ctx)));
        } else if (auto* stmt_expr_ctx =
                       dynamic_cast<ClearLanguageParser::StmtExprContext*>(else_stmt_ctx)) {
            ELSE_BLK->statements.push_back(
                std::any_cast<std::shared_ptr<StmtExpr>>(visit(stmt_expr_ctx)));
        } else if (auto* stmt_if_ctx =
                       dynamic_cast<ClearLanguageParser::StmtIfContext*>(else_stmt_ctx)) {
            ELSE_BLK->statements.push_back(
                std::any_cast<std::shared_ptr<StmtIf>>(visit(stmt_if_ctx)));
        } else {
            visit(else_stmt_ctx);
        }
        node->else_blk = ELSE_BLK;
    }

    return node;
}

std::any SemaBuilder::visitIfSingle(ClearLanguageParser::IfSingleContext* ctx) {
    const auto COND = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->expr()));

    const bool IS_BOOL = COND->type.isBuiltin() && isBoolKind(COND->type.builtin.kind);
    const bool IS_INT = COND->type.isBuiltin() && isIntKind(COND->type.builtin.kind);

    if (!(IS_BOOL || IS_INT)) {
        throw std::runtime_error("if condition must be boolean or int expression");
    }

    auto node = std::make_shared<StmtIf>();
    node->cond = COND;

    const auto THEN_BLK = std::make_shared<Block>();
    auto* then_stmt_ctx = ctx->stmt(0);
    if (auto* stmt_return_ctx =
            dynamic_cast<ClearLanguageParser::StmtReturnContext*>(then_stmt_ctx)) {
        THEN_BLK->statements.push_back(
            std::any_cast<std::shared_ptr<StmtReturn>>(visit(stmt_return_ctx)));
    } else if (auto* stmt_var_decl_ctx =
                   dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(then_stmt_ctx)) {
        THEN_BLK->statements.push_back(
            std::any_cast<std::shared_ptr<StmtVarDecl>>(visit(stmt_var_decl_ctx)));
    } else if (auto* stmt_expr_ctx =
                   dynamic_cast<ClearLanguageParser::StmtExprContext*>(then_stmt_ctx)) {
        THEN_BLK->statements.push_back(
            std::any_cast<std::shared_ptr<StmtExpr>>(visit(stmt_expr_ctx)));
    } else if (auto* stmt_if_ctx =
                   dynamic_cast<ClearLanguageParser::StmtIfContext*>(then_stmt_ctx)) {
        THEN_BLK->statements.push_back(std::any_cast<std::shared_ptr<StmtIf>>(visit(stmt_if_ctx)));
    } else {
        visit(then_stmt_ctx);
    }
    node->then_blk = THEN_BLK;

    node->else_blk = nullptr;
    const auto& blks = ctx->getRuleContexts<ClearLanguageParser::BlockContext>();
    const auto& stmts_vec = ctx->stmt();
    if (!blks.empty()) {
        node->else_blk = std::any_cast<std::shared_ptr<Block>>(visit(blks[0]));
    } else if (stmts_vec.size() >= 2) {
        const auto ELSE_BLK = std::make_shared<Block>();
        auto* else_stmt_ctx = stmts_vec[1];
        if (auto* stmt_return_ctx =
                dynamic_cast<ClearLanguageParser::StmtReturnContext*>(else_stmt_ctx)) {
            ELSE_BLK->statements.push_back(
                std::any_cast<std::shared_ptr<StmtReturn>>(visit(stmt_return_ctx)));
        } else if (auto* stmt_var_decl_ctx =
                       dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(else_stmt_ctx)) {
            ELSE_BLK->statements.push_back(
                std::any_cast<std::shared_ptr<StmtVarDecl>>(visit(stmt_var_decl_ctx)));
        } else if (auto* stmt_expr_ctx =
                       dynamic_cast<ClearLanguageParser::StmtExprContext*>(else_stmt_ctx)) {
            ELSE_BLK->statements.push_back(
                std::any_cast<std::shared_ptr<StmtExpr>>(visit(stmt_expr_ctx)));
        } else if (auto* stmt_if_ctx =
                       dynamic_cast<ClearLanguageParser::StmtIfContext*>(else_stmt_ctx)) {
            ELSE_BLK->statements.push_back(
                std::any_cast<std::shared_ptr<StmtIf>>(visit(stmt_if_ctx)));
        } else {
            visit(else_stmt_ctx);
        }
        node->else_blk = ELSE_BLK;
    }

    return node;
}

std::any SemaBuilder::visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) {
    auto* var_decl_ctx = ctx->varDecl();
    auto node = std::make_shared<StmtVarDecl>();

    std::shared_ptr<Expr> init_expr;
    TypeRef decl_ty;
    std::string name;
    sema::mutability mut = sema::mutability::CONST;

    if (auto* let = dynamic_cast<ClearLanguageParser::LetStmtDeclContext*>(var_decl_ctx)) {
        name = let->IDENT()->getText();
        decl_ty = makeTypeRefFrom(let->type());
        if (let->expr() != nullptr) {
            init_expr = std::any_cast<std::shared_ptr<Expr>>(visit(let->expr()));
        }
        mut = sema::mutability::LET;
    } else if (auto* var = dynamic_cast<ClearLanguageParser::VarStmtDeclContext*>(var_decl_ctx)) {
        name = var->IDENT()->getText();
        decl_ty = makeTypeRefFrom(var->type());
        if (var->expr() != nullptr) {
            init_expr = std::any_cast<std::shared_ptr<Expr>>(visit(var->expr()));
        }
        mut = sema::mutability::VAR;
    } else {
        throw std::runtime_error("unknown varDecl kind");
    }

    node->name = name;
    node->decl_type = decl_ty;

    auto toSemaMut = [](sema::mutability mut) -> sema::mutability {
        switch (mut) {
        case sema::mutability::CONST:
            return sema::mutability::CONST;
        case sema::mutability::LET:
            return sema::mutability::LET;
        case sema::mutability::VAR:
            return sema::mutability::VAR;
        }
        return sema::mutability::CONST;
    };
    node->mut = toSemaMut(mut);

    if (init_expr) {
        if (init_expr->type.isBuiltin() && init_expr->type.builtin.kind == Type::kind_enum::I32) {
            if (const auto LIT = std::dynamic_pointer_cast<Literal>(init_expr)) {
                if (LIT->value.is_untyped_int) {
                    const auto COERCED = sema_utils::coerceUntypedIntTo(LIT->value, decl_ty);
                    LIT->value = COERCED;
                    LIT->type = decl_ty;
                    init_expr->type = decl_ty;
                }
            }
        }
        if (init_expr->type.isBuiltin() && init_expr->type.builtin.kind != decl_ty.builtin.kind) {
            throw std::runtime_error("type mismatch in var init");
        }
    }
    node->init_expr = init_expr;

    SymbolEntry sym_entry;
    sym_entry.kind = symbol_kind::VARIABLE;
    sym_entry.type = decl_ty;
    sym_entry.mut = mut;
    if (!insertSymbol(node->name, sym_entry)) {
        throw std::runtime_error("redefinition in same scope: " + node->name);
    }
    return node;
}

std::any SemaBuilder::visitConstantDecl(ClearLanguageParser::ConstantDeclContext* ctx) {
    const auto QUALIFIED_NAME = qualify(ctx->IDENT()->getText());
    SymbolEntry sym_entry;
    sym_entry.kind = symbol_kind::CONSTANT;
    sym_entry.type = makeTypeRefFrom(ctx->type());
    sym_entry.mut = sema::mutability::CONST;

    if (ctx->constExpr() != nullptr) {
        auto expr_node = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->constExpr()));

        if (!expr_node->isConst()) {
            throw std::runtime_error("constant initializer must be a constant expression");
        }

        value_variant folded_value;
        if (!tryFoldExpr(expr_node, folded_value)) {
            throw std::runtime_error("failed to constant fold constant initializer");
        }
        sym_entry.const_expr = expr_node;
        sym_entry.value = folded_value;
    }
    if (!insertSymbol(QUALIFIED_NAME, sym_entry)) {
        throw std::runtime_error("constant redeclaration: " + QUALIFIED_NAME);
    }
    return nullptr;
}

std::any SemaBuilder::visitConstExpr(ClearLanguageParser::ConstExprContext* ctx) {
    return visit(ctx->constAddExpr());
}

std::any SemaBuilder::visitConstAddExpr(ClearLanguageParser::ConstAddExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->left));
    for (size_t i = 0; i < ctx->right.size(); ++i) {
        const auto RHS = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->right[i]));
        const std::string OPER = ctx->op[i]->getText();
        if (!cur->type.isBuiltin() || !RHS->type.isBuiltin() ||
            cur->type.builtin.kind != RHS->type.builtin.kind) {
            throw std::runtime_error("type mismatch in binary op");
        }
        if (isString(cur->type) && isString(RHS->type)) {
            if (OPER != "+") {
                throw std::runtime_error("only + operator is supported for string concatenation");
            }
        }

        const auto BIN = std::make_shared<BinOp>();
        BIN->op = OPER;
        BIN->lhs = cur;
        BIN->rhs = RHS;
        BIN->type = cur->type;
        cur = BIN;
    }
    return cur;
}

std::any SemaBuilder::visitConstMulExpr(ClearLanguageParser::ConstMulExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->left));
    for (size_t i = 0; i < ctx->right.size(); ++i) {
        const auto RHS = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->right[i]));
        const std::string OPER = ctx->op[i]->getText();
        if (!cur->type.isBuiltin() || !RHS->type.isBuiltin() ||
            cur->type.builtin.kind != RHS->type.builtin.kind) {
            throw std::runtime_error("type mismatch in binary op");
        }

        if (!isNumKind(cur->type.builtin.kind) || !isNumKind(RHS->type.builtin.kind)) {
            throw std::runtime_error("operator " + OPER + " requires numeric operand types");
        }

        const auto BIN = std::make_shared<BinOp>();
        BIN->op = OPER;
        BIN->lhs = cur;
        BIN->rhs = RHS;
        BIN->type = cur->type;
        cur = BIN;
    }
    return cur;
}

std::any SemaBuilder::visitUnaryConstMinus(ClearLanguageParser::UnaryConstMinusContext* ctx) {
    const auto INNER = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->inner));

    const auto NODE = std::make_shared<Unary>();
    NODE->op = "-";
    NODE->inner = INNER;
    NODE->type = INNER->type;
    return std::static_pointer_cast<Expr>(NODE);
}

std::any SemaBuilder::visitIntConstLiteral(ClearLanguageParser::IntConstLiteralContext* ctx) {
    const auto NODE = std::make_shared<Literal>();
    NODE->type = TypeRef::builtinType(Type{Type::kind_enum::I32});
    int64_t int64_v = std::stoll(ctx->INT()->getText());
    NODE->value = Value{NODE->type, int64_v, true};
    return std::static_pointer_cast<Expr>(NODE);
}

std::any SemaBuilder::visitFloatConstLiteral(ClearLanguageParser::FloatConstLiteralContext* ctx) {
    const auto NODE = std::make_shared<Literal>();
    NODE->type = TypeRef::builtinType(Type{Type::kind_enum::F32});
    const std::string TOK = ctx->FLOAT()->getText();
    const llvm::APFloat APF(llvm::APFloat::IEEEsingle(), TOK);
    if (APF.isInfinity()) {
        throw std::runtime_error("f32: Out of range" + TOK);
    }
    const uint32_t BITS = static_cast<uint32_t>(APF.bitcastToAPInt().getZExtValue());
    ClF32 h_v;
    h_v.bits = BITS;
    NODE->value = Value{NODE->type, h_v, true};
    return std::static_pointer_cast<Expr>(NODE);
}

std::any SemaBuilder::visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) {
    auto node = std::make_shared<StmtReturn>();
    if (ctx->expr() != nullptr) {
        node->value = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->expr()));
    }
    return node;
}

std::any SemaBuilder::visitStmtExpr(ClearLanguageParser::StmtExprContext* ctx) {
    auto node = std::make_shared<StmtExpr>();
    node->expr = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->expr()));
    return node;
}

std::any SemaBuilder::visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) {
    auto cur = std::any_cast<std::shared_ptr<Expr>>(visit(ctx->primary()));

    auto parse_func_for = [](const Type::kind_enum KEY) -> const char* {
        switch (KEY) {
        case Type::kind_enum::I8:
            return "__cl_parse_i8";
        case Type::kind_enum::U8:
            return "__cl_parse_u8";
        case Type::kind_enum::I16:
            return "__cl_parse_i16";
        case Type::kind_enum::U16:
            return "__cl_parse_u16";
        case Type::kind_enum::I32:
            return "__cl_parse_i32";
        case Type::kind_enum::U32:
            return "__cl_parse_u32";
        case Type::kind_enum::I64:
            return "__cl_parse_i64";
        case Type::kind_enum::U64:
            return "__cl_parse_u64";
        case Type::kind_enum::STRING:
        case Type::kind_enum::F16:
        case Type::kind_enum::F32:
        case Type::kind_enum::NORETURN:
        case Type::kind_enum::UNIT:
            return nullptr;
        }
        return nullptr;
    };

    for (auto* child : ctx->children) {
        if (auto* call_suffix_ctx = dynamic_cast<ClearLanguageParser::CallSuffixContext*>(child)) {
            const auto V_REF = std::dynamic_pointer_cast<VarRef>(cur);
            if (!V_REF) {
                throw std::runtime_error("can only call functions by name");
            }

            std::string callee_fqn = resolveFunctionName(V_REF->name);
            const auto* fse = lookupSymbol(callee_fqn);
            if ((fse == nullptr) || fse->kind != symbol_kind::FUNCTION) {
                throw std::runtime_error("call to undefined function: " + V_REF->name);
            }

            const auto& sig = fse->function_sig;

            const auto CALL = std::make_shared<sema::Call>();
            CALL->callee = callee_fqn;

            if (auto* const ARG_LIST = call_suffix_ctx->argList()) {
                for (auto* ectx : ARG_LIST->expr()) {
                    CALL->args.push_back(std::any_cast<std::shared_ptr<Expr>>(visit(ectx)));
                }
            }

            if (sig->param_types.size() != CALL->args.size()) {
                throw std::runtime_error("argument count mismatch in function call: " +
                                         CALL->callee);
            }

            for (size_t i = 0; i < CALL->args.size(); ++i) {
                if (!CALL->args[i]->type.isBuiltin() || !sig->param_types[i].isBuiltin() ||
                    CALL->args[i]->type.builtin.kind != sig->param_types[i].builtin.kind) {
                    throw std::runtime_error("argument type mismatch in function call: " +
                                             CALL->callee);
                }
            }
            CALL->type = *sig->return_type;

            cur = CALL;
            continue;
        }

        if (auto* as_suffix_ctx = dynamic_cast<ClearLanguageParser::AsSuffixContext*>(child)) {
            TypeRef target = makeTypeRefFrom(as_suffix_ctx->type());

            if (const auto LIT = std::dynamic_pointer_cast<Literal>(cur)) {
                if (LIT->value.is_untyped_int) {
                    const auto COERCED = sema_utils::coerceUntypedIntTo(LIT->value, target);
                    LIT->value = COERCED;
                    LIT->type = target;
                    cur->type = target;
                    continue;
                }
            }

            if (!(cur->type.isBuiltin() && target.isBuiltin())) {
                throw std::runtime_error("can only cast between builtin types");
            }

            const auto SRC_K = cur->type.builtin.kind;
            const auto DST_K = target.builtin.kind;

            if (SRC_K == DST_K) {
                cur->type = target;
                continue;
            }

            bool ok_b = false;
            if (isIntKind(SRC_K) && isIntKind(DST_K)) {
                ok_b = true;
            }
            if (isIntKind(SRC_K) && DST_K == Type::kind_enum::F16) {
                ok_b = true;
            }
            if (isIntKind(SRC_K) && DST_K == Type::kind_enum::F32) {
                ok_b = true;
            }
            if (SRC_K == Type::kind_enum::F16 && isIntKind(DST_K)) {
                ok_b = true;
            }
            if (SRC_K == Type::kind_enum::F32 && isIntKind(DST_K)) {
                ok_b = true;
            }

            if (SRC_K == Type::kind_enum::STRING || DST_K == Type::kind_enum::STRING) {
                ok_b = false;
            }

            if (!ok_b) {
                throw std::runtime_error(std::string("unsupported 'as' cast: ") +
                                         builtinTypeName(cur->type.builtin) + " -> " +
                                         builtinTypeName(target.builtin));
            }

            const auto CAST = std::make_shared<Cast>();
            CAST->inner = cur;
            CAST->target_type = target;
            CAST->type = target;
            cur = CAST;
            continue;
        }

        if (auto* asf = dynamic_cast<ClearLanguageParser::AsForceSuffixContext*>(child)) {
            TypeRef target = makeTypeRefFrom(asf->type());
            if (const auto LIT = std::dynamic_pointer_cast<Literal>(cur)) {
                if (LIT->value.is_untyped_int) {
                    const auto COERCED = sema_utils::coerceUntypedIntTo(LIT->value, target);
                    LIT->value = COERCED;
                    LIT->type = target;
                    cur->type = target;
                    continue;
                }
            }
            if (!(cur->type.isBuiltin() && target.isBuiltin())) {
                throw std::runtime_error("can only cast between builtin types");
            }
            const auto SRC_K = cur->type.builtin.kind;
            const auto DST_K = target.builtin.kind;
            if (SRC_K == DST_K) {
                cur->type = target;
                continue;
            }

            if (SRC_K == Type::kind_enum::STRING && isIntKind(DST_K)) {
                if (const auto LIT = std::dynamic_pointer_cast<Literal>(cur)) {
                    if (std::holds_alternative<std::string>(LIT->value.v)) {
                        const std::string& str = std::get<std::string>(LIT->value.v);
                        auto is_digits = [](const std::string& text) {
                            if (text.empty()) {
                                return false;
                            }
                            size_t idx = (text[0] == '+' || text[0] == '-') ? 1U : 0U;
                            if (idx >= text.size()) {
                                return false;
                            }
                            for (; idx < text.size(); ++idx) {
                                if (!std::isdigit(static_cast<unsigned char>(text[idx]))) {
                                    return false;
                                }
                            }
                            return true;
                        };
                        if (is_digits(str)) {
                            try {
                                if (TypeRef::isUnsigned(target)) {
                                    auto u_val = static_cast<uint64_t>(std::stoull(str));
                                    if (!fits(target, std::variant<int64_t, uint64_t>(u_val))) {
                                        throw std::runtime_error("overflow");
                                    }
                                    LIT->value = Value{target, u_val, false};
                                } else {
                                    auto i_val = static_cast<int64_t>(std::stoll(str));
                                    if (!fits(target, std::variant<int64_t, uint64_t>(i_val))) {
                                        throw std::runtime_error("overflow");
                                    }
                                    LIT->value = Value{target, i_val, false};
                                }
                                LIT->type = target;
                                cur->type = target;
                                continue;
                            }
                            // NOLINTNEXTLINE(bugprone-empty-catch)
                            catch (...) {
                            }
                        }
                    }
                }
                const char* callee = parse_func_for(DST_K);
                if (callee == nullptr) {
                    throw std::runtime_error(
                        "string to this target type via 'as! is not supported");
                }
                const auto CALL = std::make_shared<Call>();
                CALL->callee = callee;
                CALL->args.clear();
                CALL->args.push_back(cur);
                CALL->type = target;
                cur = CALL;
                continue;
            }

            bool ok_b = false;
            if (isIntKind(SRC_K) && isIntKind(DST_K)) {
                ok_b = true;
            }
            if (isIntKind(SRC_K) && DST_K == Type::kind_enum::F16) {
                ok_b = true;
            }
            if (isIntKind(SRC_K) && DST_K == Type::kind_enum::F32) {
                ok_b = true;
            }
            if (SRC_K == Type::kind_enum::F16 && isIntKind(DST_K)) {
                ok_b = true;
            }
            if (SRC_K == Type::kind_enum::F32 && isIntKind(DST_K)) {
                ok_b = true;
            }

            if (SRC_K == Type::kind_enum::STRING || DST_K == Type::kind_enum::STRING) {
                ok_b = false;
            }

            if (!ok_b) {
                throw std::runtime_error(std::string("unsupported 'as!' cast: ") +
                                         builtinTypeName(cur->type.builtin) + " -> " +
                                         builtinTypeName(target.builtin));
            }

            const auto CAST = std::make_shared<Cast>();
            CAST->inner = cur;
            CAST->target_type = target;
            CAST->type = target;
            cur = CAST;
            continue;
        }
    }
    return cur;
}

std::any SemaBuilder::visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) {
    return visit(ctx->expr());
}

std::any SemaBuilder::visitUnitLiteral(ClearLanguageParser::UnitLiteralContext* ctx) {
    const auto LIT = std::make_shared<Literal>();
    LIT->type = TypeRef::builtinType(Type{Type::kind_enum::UNIT});
    LIT->value = Value{LIT->type, static_cast<int64_t>(0), false};
    return std::static_pointer_cast<Expr>(LIT);
}

static std::string unescapeStringToken(const std::string& tok) {
    std::string input = tok;
    if (input.size() >= 2 && input.front() == '"' && input.back() == '"') {
        input = input.substr(1, input.size() - 2);
    }
    std::string out;
    out.reserve(input.size());
    for (size_t i = 0; i < input.size(); ++i) {
        if (input[i] == '\\' && i + 1 < input.size()) {
            switch (const char NEXT = input[i + 1]) {
            case 'b':
                out.push_back('\b');
                break;
            case 'f':
                out.push_back('\f');
                break;
            case 'n':
                out.push_back('\n');
                break;
            case 'r':
                out.push_back('\r');
                break;
            case 't':
                out.push_back('\t');
                break;
            case '\\':
                out.push_back('\\');
                break;
            case '\'':
                out.push_back('\'');
                break;
            case '"':
                out.push_back('"');
                break;
            default:
                out.push_back(NEXT);
                break;
            }
            ++i;
        } else {
            out.push_back(input[i]);
        }
    }
    return out;
}

std::any SemaBuilder::visitStringLiteral(ClearLanguageParser::StringLiteralContext* ctx) {
    const auto NODE = std::make_shared<Literal>();
    NODE->type = TypeRef::builtinType(Type{Type::kind_enum::STRING});
    const std::string RAW = ctx->STRING()->getText();
    NODE->value = makeString(unescapeStringToken(RAW));
    return std::static_pointer_cast<Expr>(NODE);
}

std::any SemaBuilder::visitBoolLiteral(ClearLanguageParser::BoolLiteralContext* ctx) {
    const auto NODE = std::make_shared<Literal>();
    NODE->type = booleanType();
    const bool IS_TRUE(ctx->TRUE() != nullptr);
    NODE->value = Value{NODE->type, static_cast<int64_t>(IS_TRUE ? 1 : 0), false};
    return std::static_pointer_cast<Expr>(NODE);
}

std::any SemaBuilder::visitStringConstLiteral(ClearLanguageParser::StringConstLiteralContext* ctx) {
    const auto NODE = std::make_shared<Literal>();
    NODE->type = TypeRef::builtinType(Type{Type::kind_enum::STRING});
    std::string raw = ctx->STRING()->getText();
    NODE->value = makeString(unescapeStringToken(raw));
    return std::static_pointer_cast<Expr>(NODE);
}

std::any SemaBuilder::visitBoolConstLiteral(ClearLanguageParser::BoolConstLiteralContext* ctx) {
    const auto NODE = std::make_shared<Literal>();
    NODE->type = booleanType();
    const bool IS_TRUE(ctx->TRUE() != nullptr);
    NODE->value = Value{NODE->type, static_cast<int64_t>(IS_TRUE ? 1 : 0), false};
    return std::static_pointer_cast<Expr>(NODE);
}
SemaBuilder::ScopeGuard::~ScopeGuard() {
    if (active) {
        s_builder.popScope();
    }
}
