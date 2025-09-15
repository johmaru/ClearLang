#include <any>
#include <optional>
#include <string>
#include <stdexcept>
#include <cstdint>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>
#include "antlr4-runtime.h"
#include "ClearLanguageLexer.h"
#include "ClearLanguageParser.h"
#include "ClearLanguageBaseVisitor.h"
#include "../core/CLType.h"
#include <cmath>

using namespace antlr4;

class EvalVisitor : public ClearLanguageBaseVisitor {

    struct FunctionDef {FunctionValue fv; ClearLanguageParser::BlockContext* body = nullptr;};

    std::vector<std::unordered_map<std::string, Value>> varScopes;
    std::vector<std::unordered_map<std::string, TypeRef>>  typeScopes;

    std::optional<TypeRef> expectedType;

    struct ReturnSignal {bool hasValue = false; Value value;};
    std::optional<TypeRef> currentFuncReturnType;

    std::unordered_map<std::string, FunctionDef> functionTable;

    static bool isUnit(const TypeRef& t) {
        return t.isBuiltin() && t.builtin.Kind == Type::UNIT;
    }
    static bool isNoReturn(const TypeRef& t) {
        return t.isBuiltin() && t.builtin.Kind == Type::NORETURN;
    }

    static std::string boundsString(const TypeRef& t) {
        if (!t.isBuiltin()) return "(non-builtin)";
        if (t.builtin.isUnsigned()) {
            auto [mn, mx] = t.builtin.unsignedBounds();
            return "[" + std::to_string(mn) + ".." + std::to_string(mx) + "]";
        } else {
            auto [mn, mx] = t.builtin.signedBounds();
            return "[" + std::to_string(mn) + ".." + std::to_string(mx) + "]";
        }
    }

    static void checkRange(const TypeRef& t, const std::variant<int64_t,uint64_t>& v) {
        if (!t.isBuiltin()) throw std::runtime_error("numeric range checked on non-builtin: " + typeName(t));
        if (!fits(t, v)) {
            std::string msg = "initializer out of range; allowed range: ";
            msg += boundsString(t);
            msg += " for type ";
            msg += typeName(t);
            throw std::runtime_error(msg);
        }
    }

    static void checkRangeValue(const TypeRef& t, const Value& v) {
        if (isUnit(v.type)) throw std::runtime_error("unit value cannot be used in expressions");
        if (!t.isBuiltin()) throw std::runtime_error("numeric range checked on non-builtin: " + typeName(t));

        if (t.builtin.Kind == Type::F16) {
            float f = 0.0f;
            if (std::holds_alternative<CLHalf>(v.v)) {
                f = static_cast<float>(std::get<CLHalf>(v.v));
            } else if (std::holds_alternative<int64_t>(v.v)) {
                f = static_cast<float>(std::get<int64_t>(v.v));
            } else if (std::holds_alternative<uint64_t>(v.v)) {
                f = static_cast<float>(std::get<uint64_t>(v.v));
            } else {
                throw std::runtime_error("non-float value cannot be used as a float");
            }
            if (!std::isfinite(f) || f < -65504.0f || f > 65504.0f) {
                std::string msg = "initializer out of range; allowed range: [-65504.0..65504.0] for type f16";
                throw std::runtime_error(msg);
            }
            // Early return for F16 so we don't fall through to integer range checks
            return;
        }

        auto nv = asNum2(v);
        checkRange(t, nv);
    }

    static Value coerceUntypedTo(const Value& val, const TypeRef& targetType) {
        if (!targetType.isBuiltin()) {
            throw std::runtime_error("cannot coerce to non-builtin type: " + typeName(targetType));
        }
        if (isUnit(targetType) || isNoReturn(targetType)) {
            throw std::runtime_error("cannot coerce integer to this type");
        }

        Value r{targetType, std::monostate{}, false};
        if (targetType.builtin.isUnsigned()) {
            uint64_t u = std::holds_alternative<uint64_t>(val.v)
                    ? std::get<uint64_t>(val.v)
                    : static_cast<uint64_t>(std::get<int64_t>(val.v));
            r.v = u;
        } else {
            int64_t s = std::holds_alternative<int64_t>(val.v)
                    ? std::get<int64_t>(val.v)
                    : static_cast<int64_t>(std::get<uint64_t>(val.v));
            r.v = s;
        }
        checkRangeValue(r.type, r);
        return r;
    }

    template <typename Op>
    static Value binOp(Value lhs, Value rhs, const std::string& opSym, Op op) {

        if (lhs.type.isBuiltin() && lhs.type.builtin.Kind == Type::NORETURN || rhs.type.isBuiltin() && rhs.type.builtin.Kind == Type::NORETURN) {
            throw std::runtime_error("noreturn value cannot be used in expressions");
        }

        if (lhs.type.isBuiltin() && lhs.type.builtin.Kind == Type::UNIT || rhs.type.isBuiltin() && rhs.type.builtin.Kind == Type::UNIT) {
            throw std::runtime_error("unit value cannot be used in expressions");
        }
    
        if (lhs.isUntypedInt && !rhs.isUntypedInt) lhs = coerceUntypedTo(lhs, rhs.type);
        if (rhs.isUntypedInt && !lhs.isUntypedInt) rhs = coerceUntypedTo(rhs, lhs.type);

        if (lhs.isUntypedInt && rhs.isUntypedInt) {
            lhs = Value{ TypeRef::builtinType(Type{Type::I32}),
             std::get<int64_t>(asNum2(lhs)), false };
            rhs = Value{TypeRef::builtinType(Type{Type::I32}),
             int64_t{std::get<int64_t>(asNum2(rhs))}, false};
        }

        if ((lhs.type.isBuiltin() && (lhs.type.builtin.Kind != rhs.type.builtin.Kind))) {
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

        if (lhs.type.builtin.isUnsigned()) {
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

    TypeRef resolveType(const std::string& name) const {
        for (auto it = typeScopes.rbegin(); it != typeScopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) return found->second;
        }
        return TypeRef::builtinType(Type::fromString(name));
    }

    void defineType(const std::string& name, const TypeRef& t) {
        auto& cur = typeScopes.back();
        if (cur.count(name)) throw std::runtime_error("type redefinition: " + name);
        cur.emplace(name, t);
    }

public:
    EvalVisitor() {
        // Initialize type scopes
        pushScopes();
        typeScopes.back().emplace("i8", TypeRef::builtinType(Type{Type::I8}));
        typeScopes.back().emplace("u8", TypeRef::builtinType(Type{Type::U8}));
        typeScopes.back().emplace("i32", TypeRef::builtinType(Type{Type::I32}));
        typeScopes.back().emplace("int", TypeRef::builtinType(Type{Type::I32}));
        typeScopes.back().emplace("u32", TypeRef::builtinType(Type{Type::U32}));
        typeScopes.back().emplace("i64", TypeRef::builtinType(Type{Type::I64}));
        typeScopes.back().emplace("u64", TypeRef::builtinType(Type{Type::U64}));
        typeScopes.back().emplace("f16", TypeRef::builtinType(Type{Type::F16}));
        typeScopes.back().emplace("noreturn", TypeRef::builtinType(Type{Type::NORETURN}));
        typeScopes.back().emplace("unit", TypeRef::builtinType(Type{Type::UNIT}));

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
        TypeRef retType = resolveType(entry->type()->getText());
        currentFuncReturnType = retType;

        for (auto* fd : ctx->funcDecl()) {
            const std::string name = fd->name->getText();
            std::vector<std::string> paramNames;
            std::vector<TypeRef> paramTypes;
            if (auto pl = fd->paramList()) {
                for (auto* p : pl->param()) {
                   paramNames.push_back(p->IDENT()->getText());
                   paramTypes.push_back(makeTypeRefFrom(p->type()));
                }
            }

            TypeRef retTr = makeTypeRefFrom(fd->type());

            auto sig = std::make_shared<FunctionSig>();
            sig->paramTypes = std::move(paramTypes);
            sig->returnType = std::make_shared<TypeRef>(retTr);

            FunctionValue fv{name, paramNames, sig };
            FunctionDef def{ std::move(fv), fd->block() };
            if (!functionTable.emplace(name, std::move(def)).second) {
                throw std::runtime_error("function redefinition: " + name);
            }
        }

        try {
            visit(entry->block());
            // For functions declared as noreturn, reaching the end without a return is valid.
            if (retType.isBuiltin() &&
                (retType.builtin.Kind == Type::NORETURN || retType.builtin.Kind == Type::UNIT)) {
                return std::any{};
            }
            throw std::runtime_error("function did not return a value");
        } catch (const ReturnSignal& rs) {
            if (retType.isBuiltin() && (retType.builtin.Kind == Type::NORETURN || retType.builtin.Kind == Type::UNIT)) {
                return std::any{};
            }
            return static_cast<std::any>(asNum2(rs.value));
        }
    }

    TypeRef makeTypeRefFrom(ClearLanguageParser::TypeContext* ctx) {
        if (auto nt = dynamic_cast<ClearLanguageParser::NamedTypeContext*>(ctx)) {
            Type bt = Type::fromString(nt->IDENT()->getText());
            return TypeRef::builtinType(bt);
        } else if (auto ut = dynamic_cast<ClearLanguageParser::UnitTypeContext*>(ctx)) {
            return TypeRef::builtinType(Type{Type::UNIT});
        } else if (auto ft = dynamic_cast<ClearLanguageParser::FunctionTypeContext*>(ctx)) {
            auto sig = std::make_shared<FunctionSig>();
            if (auto tl = ft->typeList()) {
                for (auto* tctx : tl->type()) {
                    sig->paramTypes.push_back(makeTypeRefFrom(tctx));
                }
            }
            sig->returnType = std::make_shared<TypeRef>(makeTypeRefFrom(ft->type()));
            return TypeRef::functionType(std::move(sig));
        }
        throw std::runtime_error("unknown type alt: " + ctx->getText());
    }

    std::any visitUnitLiteral(ClearLanguageParser::UnitLiteralContext* ctx) override {
        return Value{ TypeRef::builtinType(Type{Type::UNIT}), std::monostate{}, false };
    }

    std::any visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) override {
        if (!currentFuncReturnType.has_value()) {
            throw std::runtime_error("internal: function return type not set");
        }

        if (isNoReturn(*currentFuncReturnType)) {
            throw std::runtime_error("noreturn function cannot return");
        }

        if (isUnit(*currentFuncReturnType)) {
            if (!ctx->expr()) {
                throw ReturnSignal{false, {TypeRef::builtinType(Type{Type::UNIT}), std::monostate{}, false}};
            }
            auto oldExpected = expectedType;
            expectedType = currentFuncReturnType;
            Value v = std::any_cast<Value>(visit(ctx->expr()));
            expectedType = oldExpected;

            if (v.type.isBuiltin() && v.type.builtin.Kind != Type::UNIT) {
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

        if (v.type.isBuiltin() && isNoReturn(v.type)) {
            throw std::runtime_error("noreturn value cannot be returned");
        }
        if (v.type.isBuiltin() && isUnit(v.type)) {
            throw std::runtime_error("unit value cannot be returned");
        }

        expectedType = oldExpected;

        Value out = v;
        if (v.isUntypedInt) {
            out = coerceUntypedTo(v, *currentFuncReturnType);
        } else if (v.type.isBuiltin() && v.type.builtin.Kind != currentFuncReturnType->builtin.Kind) {
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
        TypeRef t = resolveType(vd->type()->getText());

        // Currently, our language compiler doesn't have a function call variable, so until implement the function call variable, noreturn type is not allowed.
        if (isNoReturn(t)) {
            throw std::runtime_error("variable cannot have type 'noreturn'");
        }

        if (isUnit(t)) {
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
                if (init.type.isBuiltin() && init.type.builtin.Kind != t.builtin.Kind) {
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
            defineVar(name, t.builtin.isUnsigned() ? Value{t, uint64_t{0}, false}
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

        if (isNoReturn(inner.type)) {
            throw std::runtime_error("noreturn value cannot be used in expressions");
        }

        if (isUnit(inner.type)) {
            throw std::runtime_error("unit value cannot be used in expressions");
        }

        if (inner.isUntypedInt) {
            int64_t s = -(std::holds_alternative<int64_t>(inner.v)
                         ? std::get<int64_t>(inner.v)
                         : static_cast<int64_t>(std::get<uint64_t>(inner.v)));
            return Value{TypeRef::builtinType(Type{Type::I32}), s, false};
        }
        
        if (inner.type.isBuiltin() && inner.type.builtin.Kind == Type::U8) {
            // Now unsupported UnaryMinus for U8
            throw std::runtime_error("type mismatch: cannot negate unsigned type");
        }

        if (inner.type.isBuiltin() && inner.type.builtin.Kind == Type::F16) {
            if (!std::holds_alternative<CLHalf>(inner.v)) {
                throw std::runtime_error("type mismatch: expected f16 value");
            }
            CLHalf f = std::get<CLHalf>(inner.v);
            CLHalf res;
            res.bits = static_cast<uint16_t>(f.bits ^ 0x8000u);
            return Value{inner.type, res, false};
        }


        int64_t a = std::holds_alternative<int64_t>(inner.v) ? std::get<int64_t>(inner.v) : static_cast<int64_t>(std::get<uint64_t>(inner.v));
        int64_t res = -a;
        checkRange(inner.type, std::variant<int64_t, uint64_t>{res});
        return Value{inner.type, int64_t{res}, false};
    }

    std::any visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) override {

        std::string identName;
        if (auto vr = dynamic_cast<ClearLanguageParser::VarRefContext*>(ctx->primary())) {
            identName = vr->IDENT()->getText();
        }

        if (!ctx->callSuffix().empty()) {
            if (!identName.empty()) {
                auto tryCallByName = [&](const std::string& ident, ClearLanguageParser::CallSuffixContext* cs) -> Value {
                    auto it = functionTable.find(ident);
                    if (it == functionTable.end()) {
                        throw std::runtime_error("undefined function: " + ident);
                    }
                    std::vector<Value> args;
                    if (auto al = cs->argList()) {
                        const auto& defRef = it->second;
                        const auto& paramTs = defRef.fv.sig->paramTypes;

                        for (size_t i = 0; i < al->expr().size(); ++i) {
                            auto saved = expectedType;
                            if (i < paramTs.size() && paramTs[i].isBuiltin()) {
                                expectedType = paramTs[i];
                            }
                            Value v = std::any_cast<Value>(visit(al->expr(i)));
                            expectedType = saved;
                            args.push_back(v);
                        }
                    }
                    return callFunction(it->second, args);
                };

                Value cur{};
                for (auto* cs : ctx->callSuffix()) {

                    cur = tryCallByName(identName, cs);
                    identName.clear();
                }
                return cur;
            } else {
                // Example: (f)() is not supported now.
                throw std::runtime_error("call to non-function");
            }
        }

        Value cur = std::any_cast<Value>(visit(ctx->primary()));
        return cur;
    }

    std::any visitUnaryPrimary(ClearLanguageParser::UnaryPrimaryContext* ctx) override {
        return visitChildren(ctx);
    }
    
    std::any visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) override {
        const std::string txt = ctx->FLOAT()->getText();
        if (expectedType && isUnit(*expectedType)) {
            throw std::runtime_error("cannot use float literal to initialize unit type");
        }
        float f = std::stof(txt);
        if (expectedType.has_value()) {
            if (expectedType->isBuiltin() && expectedType->builtin.Kind == Type::F16) {
                if (!std::isfinite(f) || f < -65504.0f || f > 65504.0f) {
                    std::string msg = std::string("initializer out of range: ") + txt + "; allowed range: [-65504.0..65504.0] for type f16";
                    throw std::runtime_error(msg);
                }
                return Value{*expectedType, CLHalf{f}, false};
            } else {
                throw std::runtime_error("only f16 type is supported for float literals");
            }
        }
        // Default to f16 if no expected type
        if (!std::isfinite(f) || f < -65504.0f || f > 65504.0f) {
            std::string msg = std::string("initializer out of range: ") + txt + "; allowed range: [-65504.0..65504.0] for type f16";
            throw std::runtime_error(msg);
        }
        return Value{TypeRef::builtinType(Type{Type::F16}), CLHalf{f}, false};
    }

   std::any visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) override {
        const std::string txt = ctx->INT()->getText();
        if (expectedType && isUnit(*expectedType)) { int64_t v = stoll(txt); return Value{TypeRef::builtinType(Type{Type::I32}), v, true}; }

        if (expectedType.has_value()) {
            if (expectedType->builtin.isUnsigned()) {
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
        return Value{TypeRef::builtinType(Type{Type::I32}), v, true};
    }

    std::any visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) override {
        return visit(ctx->expr());
    }

    static bool isUnsigned(const Type& t){
        return t.Kind == Type::U8 || t.Kind == Type::U32 || t.Kind == Type::U64;
    }

    static void normalizeValueStorage(Value& val){
        if (val.type.isBuiltin() && isUnsigned(val.type.builtin)) {
            if (std::holds_alternative<int64_t>(val.v))
                val.v = static_cast<uint64_t>(std::get<int64_t>(val.v));
        } else {
            if (std::holds_alternative<uint64_t>(val.v))
                val.v = static_cast<int64_t>(std::get<uint64_t>(val.v));
        }
    }

private:

    static TypeRef requireBuiltinReturn(const FunctionDef& def) {
        const auto& ret = *def.fv.sig->returnType;
        if (!ret.isBuiltin()) {
            throw std::runtime_error("non-builtin return types are not supported yet");
        }
        return ret;
    }

    Value callFunction(const FunctionDef& def, const std::vector<Value>& args) {

        if (args.size() != def.fv.paramNames.size()) {
            throw std::runtime_error("argument count mismatch");
        }
        if (def.fv.sig->paramTypes.size() != def.fv.paramNames.size()) {
            throw std::runtime_error("internal: paramTypes/paramNames size mismatch");
        }

        pushScopes();
        for (size_t i = 0; i < args.size(); ++i) {
            const auto& tr = def.fv.sig->paramTypes[i];
            if (!tr.isBuiltin()) {
                throw std::runtime_error("non-builtin parameter types are not supported yet");
            }
            Value arg = args[i];
            const TypeRef& pt = tr;

            if (arg.isUntypedInt) {
                arg = coerceUntypedTo(arg, pt);
            } else if (arg.type.builtin.Kind != pt.builtin.Kind) {
                std::string msg = "argument type mismatch at #" + std::to_string(i)
                                + ": expected " + typeName(tr)
                                + ", got " + typeName(arg.type);
                throw std::runtime_error(msg);
            } else {
                checkRangeValue(pt, arg);
            }
            normalizeValueStorage(arg);
            defineVar(def.fv.paramNames[i], arg);
        }

        auto saved = currentFuncReturnType;
        TypeRef retTy = requireBuiltinReturn(def);
        currentFuncReturnType = retTy;

        try {
            visit(def.body);
            popScopes();
            currentFuncReturnType = saved;

            if (isUnit(retTy) || isNoReturn(retTy)) {
                return Value{ TypeRef::builtinType(Type{Type::UNIT}), std::monostate{}, false };
            }
            throw std::runtime_error("function did not return a value");
        } catch (const ReturnSignal& rs) {
            popScopes();
            currentFuncReturnType = saved;

            if (!rs.hasValue || isUnit(rs.value.type) || isNoReturn(rs.value.type)) {
                return Value{ TypeRef::builtinType(Type{Type::UNIT}), std::monostate{}, false };
            }
            return rs.value;
        }
    }
};