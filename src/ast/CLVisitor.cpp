#include <iostream>
#include <string>
#include <stdexcept>
#include <cstdint>
#include <unordered_map>
#include <vector>
#include "antlr4-runtime.h"
#include "ClearLanguageLexer.h"
#include "ClearLanguageParser.h"
#include "ClearLanguageBaseVisitor.h"
#include "../core/CLType.h"

using namespace antlr4;

class EvalVisitor : public ClearLanguageBaseVisitor {

    std::vector<std::unordered_map<std::string, Value>> varScopes;
    std::vector<std::unordered_map<std::string, Type>>  typeScopes;

    static const char* typeName(const Type& t) {
        switch (t.Kind) {
            case Type::U8: return "u8";
            case Type::I32: return "i32";
            case Type::I64: return "i64";
        }
        return "?";
    }

    static void checkRange(const Type& t, int64_t v) {
        if (v < t.min() || v > t.max())
            throw std::runtime_error("initializer out of range");
    }

    static Value coerceUntypedTo(const Value& val, const Type& targetType) {
        Value r{targetType, val.v, false};
        checkRange(targetType, r.v);
        return r;
    }

    template <typename Op>
    static Value binOp(Value lhs, Value rhs, const std::string& opSym, Op op) {
    
        if (lhs.isUntypedInt && !rhs.isUntypedInt) lhs = coerceUntypedTo(lhs, rhs.type);
        if (rhs.isUntypedInt && !lhs.isUntypedInt) rhs = coerceUntypedTo(rhs, lhs.type);

        if (lhs.isUntypedInt && rhs.isUntypedInt) {
            lhs = Value{Type{Type::I32}, lhs.v, false};
            rhs = Value{Type{Type::I32}, rhs.v, false};
        }

        if (lhs.type.Kind != rhs.type.Kind) {
            std::string msg = "type mismatch: ";
            msg += typeName(lhs.type);
            msg += " ";
            msg += opSym;
            msg += " ";
            msg += typeName(rhs.type);
            throw std::runtime_error(msg);
        }

        int64_t res = op(lhs.v, rhs.v);

        checkRange(lhs.type, res);
        return Value{lhs.type, res, false};
    }

    void pushScopes() {
        varScopes.emplace_back();
        typeScopes.emplace_back();
    }

    void popScopes() {
        if (!varScopes.empty()) varScopes.pop_back();
        if (!typeScopes.empty()) typeScopes.pop_back();
    }

    void defineVar(const std::string& name, const Value& val) {
        auto& cur = varScopes.back();
        if (cur.count(name)) throw std::runtime_error("redefinition: " + name);
        cur.emplace(name, val);
    }

    const Value* lookupVar(const std::string& name) const {
        for (auto it = varScopes.rbegin(); it != varScopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) return &found->second;
        }
        return nullptr;
    }

    Type resolveType(const std::string& name) const {
        for (auto it = typeScopes.rbegin(); it != typeScopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) return found->second;
        }
        return Type::fromString(name);
    }

    void defineType(const std::string& name, const Type& t) {
        auto& cur = typeScopes.back();
        if (cur.count(name)) throw std::runtime_error("type redefinition: " + name);
        cur.emplace(name, t);
    }

public:
    EvalVisitor() {
        pushScopes();
        typeScopes.back().emplace("u8", Type{Type::U8});
        typeScopes.back().emplace("i32", Type{Type::I32});
        typeScopes.back().emplace("int", Type{Type::I32});
        typeScopes.back().emplace("i64", Type{Type::I64});
    }
    std::any visitStart(ClearLanguageParser::StartContext* ctx) override {
        ClearLanguageParser::FuncDeclContext* entry = nullptr;
        for(auto* fd: ctx->funcDecl()) {
            bool hasEntry = false;
            for (auto* attrs : fd->attributes()) {
                for (auto* idTok : attrs->IDENT()) {
                    if (idTok->getText() == "EntryPoint") {
                        hasEntry = true;
                        break;
                    }
                }
            }
            if (hasEntry) {
                if (entry) throw std::runtime_error("multiple [EntryPoint] functions");
                entry = fd;
            }
        }
        if (!entry) throw std::runtime_error("No entry point found");
        return visit(entry->block());
    }

    std::any visitBlock(ClearLanguageParser::BlockContext* ctx) override {
        pushScopes();
        int64_t last = 0;
        bool hasExpr = false;

        for (auto* s : ctx->stmt()) {
            auto res = visit(s);
            if (res.type() == typeid(int64_t)) {
                last = std::any_cast<int64_t>(res);
                hasExpr = true;
            }
        }
        popScopes();
        return hasExpr ? last : static_cast<int64_t>(0);
    }

    std::any visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) override {
        auto* vd = ctx->varDecl();
        const std::string name = vd->IDENT()->getText();
        Type t = resolveType(vd->type()->getText());

        int64_t v = 0;
        if (vd->expr()) {
            auto anyInit = visit(vd->expr());
            const Value init = std::any_cast<Value>(anyInit);

            Value finalVal;
            if (init.isUntypedInt) {
                finalVal = coerceUntypedTo(init, t);
            } else {
                if (init.type.Kind != t.Kind) {
                    std::string msg = "type mismatch in initialization of ";
                    msg += name;
                    msg += ": expected ";
                    msg += typeName(t);
                    msg += ", got ";
                    msg += typeName(init.type);
                    throw std::runtime_error(msg);
                }
                checkRange(t, init.v);
                finalVal = init;
            }
            defineVar(name, finalVal);
        } else {
            defineVar(name, Value{t, 0,false}); // default initialization
        }
        
        return nullptr;
    }

    std::any visitVarRef(ClearLanguageParser::VarRefContext* ctx) override {
        const std::string name = ctx->IDENT()->getText();
        if (const Value* found = lookupVar(name)) {
            return *found;
        }
        throw std::runtime_error("undefined variable: " + name);
    }

    std::any visitStmtExpr(ClearLanguageParser::StmtExprContext* ctx) override {
        return visit(ctx->expr());
    }

    std::any visitAddExpr(ClearLanguageParser::AddExprContext* ctx) override {
        Value value = std::any_cast<Value>(visit(ctx->mulExpr(0)));

        size_t nextMulIndex = 1;
        const size_t childCount = ctx->children.size();
        for (size_t i = 1; i < childCount && nextMulIndex < ctx->mulExpr().size(); ++i) {
            auto* child = ctx->children[i]; // ← getChild(i) ではなく children[i]
            if (auto* tn = dynamic_cast<antlr4::tree::TerminalNode*>(child)) {
                const std::string op = tn->getSymbol()->getText(); // "+" or "-"
                Value rhs = std::any_cast<Value>(visit(ctx->mulExpr(nextMulIndex++)));
                if (op == "+"){
                    value = binOp(value, rhs, "+", [](int64_t a, int64_t b) { return a + b; });
                } else if (op == "-") {
                    value = binOp(value, rhs, "-", [](int64_t a, int64_t b) { return a - b; });
                }
            }
        }
        return value;
    }

    std::any visitMulExpr(ClearLanguageParser::MulExprContext* ctx) override {
        Value value = std::any_cast<Value>(visit(ctx->unaryExpr(0)));

        size_t nextUnaryIndex = 1;
        const size_t childCount = ctx->children.size();
        for (size_t i = 1; i < childCount && nextUnaryIndex < ctx->unaryExpr().size(); ++i) {
            auto* child = ctx->children[i]; // ← getChild(i) ではなく children[i]
            if (auto* tn = dynamic_cast<antlr4::tree::TerminalNode*>(child)) {
                const std::string op = tn->getSymbol()->getText(); // "*" or "/"
                Value rhs = std::any_cast<Value>(visit(ctx->unaryExpr(nextUnaryIndex++)));
                if (op == "*") value = binOp(value, rhs, "*", [](int64_t a, int64_t b) { return a * b; });
                else if (op == "/") {
                    if (rhs.v == 0) throw std::runtime_error("division by zero");
                    value = binOp(value, rhs, "/", [](int64_t a, int64_t b) { return a / b; });
                }
            }
        }
        return value;
    }

    std::any visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) override {
        Value inner = std::any_cast<Value>(visit(ctx->inner));
        if (inner.isUntypedInt) inner = Value{Type{Type::I32}, -inner.v, false};
        
        if (inner.type.Kind == Type::U8) {
            // Now unsupported UnaryMinus for U8
            throw std::runtime_error("type mismatch: cannot negate unsigned type");
        }
        int64_t res = -inner.v;
        checkRange(inner.type, res);
        return Value{inner.type, res, false};
    }

    std::any visitUnaryPrimary(ClearLanguageParser::UnaryPrimaryContext* ctx) override {
        return visitChildren(ctx);
    }

    std::any visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) override {
        int64_t v = static_cast<int64_t>(std::stoll(ctx->INT()->getText()));
        return Value{Type{Type::I32}, v, true};
    }

    std::any visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) override {
        return visit(ctx->expr());
    }
};