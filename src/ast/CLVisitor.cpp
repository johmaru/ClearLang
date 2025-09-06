#include <any>
#include <iostream>
#include <optional>
#include <string>
#include <stdexcept>
#include <cstdint>
#include <unordered_map>
#include <variant>
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

    std::optional<Type> expectedType;

    struct ReturnSignal {bool hasValue = false; Value value;};
    std::optional<Type> currentFuncReturnType;

    static const char* typeName(const Type& t) {
        switch (t.Kind) {
            case Type::I8: return "i8";
            case Type::U8: return "u8";
            case Type::I32: return "i32";
            case Type::U32: return "u32";
            case Type::I64: return "i64";
            case Type::U64: return "u64";
            case Type::NORETURN: return "noreturn";
            case Type::UNIT: return "unit";
        }
        return "?";
    }

    static std::string boundsString(const Type& t) {
        if (t.isUnsigned()) {
            auto [mn, mx] = t.unsignedBounds();
            return "[" + std::to_string(mn) + ".." + std::to_string(mx) + "]";
        } else {
            auto [mn, mx] = t.signedBounds();
            return "[" + std::to_string(mn) + ".." + std::to_string(mx) + "]";
        }
    }

    static void checkRange(const Type& t, const std::variant<int64_t,uint64_t>& v) {
        if (!t.fits(v)) {
            std::string msg = "initializer out of range: ";
            if (std::holds_alternative<int64_t>(v))
                msg += std::to_string(std::get<int64_t>(v));
            else
                msg += std::to_string(std::get<uint64_t>(v));
            msg += " allowed range: ";
            msg += boundsString(t);
            msg += " for type ";
            msg += typeName(t);
            throw std::runtime_error(msg);
        }
    }

    static void checkRangeValue(const Type& t, const Value& v) {
        if (v.type.Kind == Type::UNIT) {
            throw std::runtime_error("unit value cannot be used in expressions");
        }
        auto nv = asNum2(v);
        if (!t.fits(nv)) {
            std::string msg = "initializer out of range: ";
            if (std::holds_alternative<int64_t>(nv)) msg += std::to_string(std::get<int64_t>(nv));
            else msg += std::to_string(std::get<uint64_t>(nv));
            msg += " for type ";
            msg += typeName(t);
            throw std::runtime_error(msg);
        }
    }

    static Value coerceUntypedTo(const Value& val, const Type& targetType) {

        if (targetType.Kind == Type::UNIT || targetType.Kind == Type::NORETURN) throw std::runtime_error("cannot coerce integer to this type");

        Value r{targetType, {}, false};
        if (isUnsigned(targetType)) {
            uint64_t u = std::holds_alternative<uint64_t>(val.v) ? std::get<uint64_t>(val.v)
                            : static_cast<uint64_t>(std::get<int64_t>(val.v));
            r.v = u;
        } else {
            int64_t s = std::holds_alternative<int64_t>(val.v) ? std::get<int64_t>(val.v)
                            : static_cast<int64_t>(std::get<uint64_t>(val.v));
            r.v = s;
        }
        checkRangeValue(r.type, r);
        return r;
    }

    template <typename Op>
    static Value binOp(Value lhs, Value rhs, const std::string& opSym, Op op) {

        if (lhs.type.Kind == Type::NORETURN || rhs.type.Kind == Type::NORETURN) {
            throw std::runtime_error("noreturn value cannot be used in expressions");
        }

        if (lhs.type.Kind == Type::UNIT || rhs.type.Kind == Type::UNIT) {
            throw std::runtime_error("unit value cannot be used in expressions");
        }
    
        if (lhs.isUntypedInt && !rhs.isUntypedInt) lhs = coerceUntypedTo(lhs, rhs.type);
        if (rhs.isUntypedInt && !lhs.isUntypedInt) rhs = coerceUntypedTo(rhs, lhs.type);

        if (lhs.isUntypedInt && rhs.isUntypedInt) {
            lhs = Value{Type{Type::I32}, std::get<int64_t>(asNum2(lhs)), false};
            rhs = Value{Type{Type::I32}, std::get<int64_t>(asNum2(rhs)), false};
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

        normalizeValueStorage(lhs);
        normalizeValueStorage(rhs);

        if (isUnsigned(lhs.type)) {
            uint64_t a = std::holds_alternative<uint64_t>(lhs.v) ? std::get<uint64_t>(lhs.v) : static_cast<uint64_t>(std::get<int64_t>(lhs.v));
            uint64_t b = std::holds_alternative<uint64_t>(rhs.v) ? std::get<uint64_t>(rhs.v) : static_cast<uint64_t>(std::get<int64_t>(rhs.v));
            
            uint64_t r = 0;
            if (opSym == "+") r = a + b;
            else if (opSym == "-") r = a - b;
            else if (opSym == "*") r = a * b;
            else if (opSym == "/") r = a / b;
            else r = op(a, b);

            checkRange(lhs.type, std::variant<int64_t,uint64_t>{r});
            return Value{lhs.type, uint64_t{r}, false};
        } else {
            int64_t a = std::holds_alternative<int64_t>(lhs.v) ? std::get<int64_t>(lhs.v) : static_cast<int64_t>(std::get<uint64_t>(lhs.v));
            int64_t b = std::holds_alternative<int64_t>(rhs.v) ? std::get<int64_t>(rhs.v) : static_cast<int64_t>(std::get<uint64_t>(rhs.v));

            int64_t r = 0;
            if (opSym == "+") r = a + b;
            else if (opSym == "-") r = a - b;
            else if (opSym == "*") r = a * b;
            else if (opSym == "/") r = a / b;
            else r = op(a, b);

            checkRange(lhs.type, std::variant<int64_t,uint64_t>{r});
            return Value{lhs.type, int64_t{r}, false};
        }
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
        // Initialize type scopes
        pushScopes();
        typeScopes.back().emplace("i8", Type{Type::I8});
        typeScopes.back().emplace("u8", Type{Type::U8});
        typeScopes.back().emplace("i32", Type{Type::I32});
        typeScopes.back().emplace("int", Type{Type::I32});
        typeScopes.back().emplace("u32", Type{Type::U32});
        typeScopes.back().emplace("i64", Type{Type::I64});
        typeScopes.back().emplace("u64", Type{Type::U64});
        typeScopes.back().emplace("noreturn", Type{Type::NORETURN});
        typeScopes.back().emplace("unit", Type{Type::UNIT});

        // End initialization type scopes
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
        Type retType = resolveType(entry->type()->getText());
        currentFuncReturnType = retType;

        try {
            visit(entry->block());
            // For functions declared as noreturn, reaching the end without a return is valid.
            if (retType.Kind == Type::NORETURN || retType.Kind == Type::UNIT) {
                return std::any{}; // no value
            }
            throw std::runtime_error("function did not return a value");
        } catch (const ReturnSignal& rs) {
            if (retType.Kind == Type::NORETURN || retType.Kind == Type::UNIT) {
                return std::any{};
            }
            return static_cast<std::any>(asNum2(rs.value));
        }
    }

    std::any visitUnitLiteral(ClearLanguageParser::UnitLiteralContext* ctx) override {
        return Value{Type{Type::UNIT}, std::monostate{}, false};
    }

    std::any visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) override {
        if (!currentFuncReturnType.has_value()) {
            throw std::runtime_error("internal: function return type not set");
        }

        if (currentFuncReturnType->Kind == Type::NORETURN) {
            throw std::runtime_error("noreturn function cannot return");
        }

        if (currentFuncReturnType->Kind == Type::UNIT) {
            if (!ctx->expr()) {
                throw ReturnSignal{false, {Type{Type::UNIT}, std::monostate{}, false}};
            }
            auto oldExpected = expectedType;
            expectedType = currentFuncReturnType;
            Value v = std::any_cast<Value>(visit(ctx->expr()));
            expectedType = oldExpected;

            if (v.type.Kind != Type::UNIT) {
                throw std::runtime_error(std::string("return type mismatch: expected unit, got ") + typeName(v.type));
            }
            throw ReturnSignal{false, v};
        }

        if (!ctx->expr()) {
            // Now didn't accept void return
            throw std::runtime_error("return value required");
        }

        auto oldExpected = expectedType;
        expectedType = currentFuncReturnType;
        Value v = std::any_cast<Value>(visit(ctx->expr()));

        if (v.type.Kind == Type::NORETURN) {
            throw std::runtime_error("noreturn value cannot be returned");
        }
        if (v.type.Kind == Type::UNIT) {
            throw std::runtime_error("unit value cannot be returned");
        }

        expectedType = oldExpected;

        Value out = v;
        if (v.isUntypedInt) {
            out = coerceUntypedTo(v, *currentFuncReturnType);
        } else if (v.type.Kind != currentFuncReturnType->Kind) {
            std::string msg = "return type mismatch: expected ";
            msg += typeName(*currentFuncReturnType);
            msg += ", got ";
            msg += typeName(v.type);
            throw std::runtime_error(msg);
        } else {
            checkRangeValue(*currentFuncReturnType, v);
        }

        throw ReturnSignal{true, out};
    }

    std::any visitBlock(ClearLanguageParser::BlockContext* ctx) override {
        pushScopes();
        try {
            for (auto* s : ctx->stmt()) {
                visit(s);
            }
            popScopes();
            return nullptr;
        } catch (...) {
            popScopes();
            throw;
        }
    }

    std::any visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) override {
        auto* vd = ctx->varDecl();
        const std::string name = vd->IDENT()->getText();
        Type t = resolveType(vd->type()->getText());

        // Currently, our language compiler doesn't have a function call variable, so until implement the function call variable, noreturn type is not allowed.
        if (t.Kind == Type::NORETURN) {
            throw std::runtime_error("variable cannot have type 'noreturn'");
        }

        if (t.Kind == Type::UNIT) {
            throw std::runtime_error("variable cannot have type 'unit'");
        }

        int64_t v = 0;
        if (vd->expr()) {

            auto oldExpectedType = expectedType;

            expectedType = t;

            auto anyInit = visit(vd->expr());
            expectedType = oldExpectedType;
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
                checkRangeValue(t, init);
                finalVal = init;
            }
            normalizeValueStorage(finalVal);
            defineVar(name, finalVal);
        } else {
            // unsigned type 0u、signed type 0
            defineVar(name, isUnsigned(t) ? Value{t, uint64_t{0}, false}
                                          : Value{t, int64_t{0},  false});
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
        visit(ctx->expr());
        return nullptr;
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
                    value = binOp(value, rhs, "+", [](auto a, auto b) { return a + b; });
                } else if (op == "-") {
                    value = binOp(value, rhs, "-", [](auto a, auto b) { return a - b; });
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
                if (op == "*") value = binOp(value, rhs, "*", [](auto a, auto b) { return a * b; });
                else if (op == "/") {
                    bool isZero = (std::holds_alternative<int64_t>(rhs.v)
                                  ? (std::get<int64_t>(rhs.v) == 0)
                                  : (std::get<uint64_t>(rhs.v) == 0));
                    if (isZero) throw std::runtime_error("division by zero");
                    value = binOp(value, rhs, "/", [](auto a, auto b) { return a / b; });
                }
            }
        }
        return value;
    }

    std::any visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) override {

        Value inner = std::any_cast<Value>(visit(ctx->inner));

        if (inner.type.Kind == Type::NORETURN) {
            throw std::runtime_error("noreturn value cannot be used in expressions");
        }

        if (inner.type.Kind == Type::UNIT) {
            throw std::runtime_error("unit value cannot be used in expressions");
        }

        if (inner.isUntypedInt) {
            int64_t s = -(std::holds_alternative<int64_t>(inner.v)
                         ? std::get<int64_t>(inner.v)
                         : static_cast<int64_t>(std::get<uint64_t>(inner.v)));
            return Value{Type{Type::I32}, int64_t{s}, false};
        }
        
        if (inner.type.Kind == Type::U8) {
            // Now unsupported UnaryMinus for U8
            throw std::runtime_error("type mismatch: cannot negate unsigned type");
        }
        int64_t a = std::holds_alternative<int64_t>(inner.v) ? std::get<int64_t>(inner.v) : static_cast<int64_t>(std::get<uint64_t>(inner.v));
        int64_t res = -a;
        checkRange(inner.type, std::variant<int64_t, uint64_t>{res});
        return Value{inner.type, int64_t{res}, false};
    }

    std::any visitUnaryPrimary(ClearLanguageParser::UnaryPrimaryContext* ctx) override {
        return visitChildren(ctx);
    }

   std::any visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) override {
        const std::string txt = ctx->INT()->getText();

        if (expectedType && expectedType->Kind == Type::UNIT) { int64_t v = stoll(txt); return Value{Type{Type::I32}, v, true}; }

        if (expectedType.has_value()) {
            if (isUnsigned(*expectedType)) {
                uint64_t uv = static_cast<uint64_t>(std::stoull(txt));
                Value val{*expectedType, uv, false};
                checkRangeValue(val.type, val);
                return val;
            } else {
                // parse as unsigned first to avoid std::stoll overflow, then check/cast
                uint64_t uv = static_cast<uint64_t>(std::stoull(txt));
                if (uv > static_cast<uint64_t>((std::numeric_limits<int64_t>::max)())) {
                    std::string msg = std::string("initializer out of range: ") + txt + "; allowed range: " + boundsString(*expectedType) + " for type " + typeName(*expectedType);
                    throw std::runtime_error(msg);
                }
                int64_t sv = static_cast<int64_t>(uv);
                Value val{*expectedType, sv, false};
                checkRangeValue(val.type, val);
                return val;
            }
        }
        int64_t v = static_cast<int64_t>(std::stoll(txt));
        return Value{Type{Type::I32}, v, true};
    }

    std::any visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) override {
        return visit(ctx->expr());
    }

    static bool isUnsigned(const Type& t){
        return t.Kind == Type::U8 || t.Kind == Type::U32 || t.Kind == Type::U64;
    }

    static void normalizeValueStorage(Value& val){
        if (isUnsigned(val.type)) {
            if (std::holds_alternative<int64_t>(val.v))
                val.v = static_cast<uint64_t>(std::get<int64_t>(val.v));
        } else {
            if (std::holds_alternative<uint64_t>(val.v))
                val.v = static_cast<int64_t>(std::get<uint64_t>(val.v));
        }
    }
};