#include <variant>
#define CLTYPE_H
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string>
#include <memory>
#include <vector>
#include <sstream>

struct Type {
    enum Kind { I8, U8, I32, U32, I64, U64, NORETURN, UNIT } Kind;
    bool isUnsigned() const {
        return Kind == U8 || Kind == U32 || Kind == U64;
    }

    std::pair<int64_t, int64_t> signedBounds() const {
        switch (Kind) {
            case I8: return {std::numeric_limits<int8_t>::min(), std::numeric_limits<int8_t>::max()};
            case I32: return {std::numeric_limits<int32_t>::min(), std::numeric_limits<int32_t>::max()};
            case I64: return {std::numeric_limits<int64_t>::min(), std::numeric_limits<int64_t>::max()};
            default: throw std::runtime_error("type is not signed");
        }
    }

    std::pair<uint64_t, uint64_t> unsignedBounds() const {
        switch (Kind) {
            case U8: return {std::numeric_limits<uint8_t>::min(), std::numeric_limits<uint8_t>::max()};
            case U32: return {std::numeric_limits<uint32_t>::min(), std::numeric_limits<uint32_t>::max()};
            case U64: return {std::numeric_limits<uint64_t>::min(), std::numeric_limits<uint64_t>::max()};
            default: throw std::runtime_error("type is not unsigned");
        }
    }

    bool fits(const std::variant<int64_t,uint64_t>& v) const {
        if (std::holds_alternative<int64_t>(v)) {
            if (isUnsigned()) return false;
            auto [minV, maxV] = signedBounds();
            int64_t iv = std::get<int64_t>(v);
            return iv >= minV && iv <= maxV;
        } else {
            if (!isUnsigned()) return false;
            auto [minV, maxV] = unsignedBounds();
            uint64_t uv = std::get<uint64_t>(v);
            return uv >= minV && uv <= maxV;
        }
    }

    static Type fromString(const std::string& s) {
        if (s == "u8") return {U8};
        if (s == "i8") return {I8};
        if (s == "i32" ||s == "int") return {I32};
        if (s == "u32") return {U32};
        if (s == "i64") return {I64};
        if (s == "u64") return {U64};
        if (s == "noreturn") return {NORETURN};
        if (s == "unit" || s == "()") return {UNIT};
        throw std::runtime_error("unknown type: " + s);
    }
};

struct FunctionSig;

struct TypeRef {
    enum class Tag { BUILTIN, FUNCTION } tag = Tag::BUILTIN;

    Type builtin{Type::I32}; // default

    std::shared_ptr<FunctionSig> funcSig;

    static TypeRef builtinType(Type t) {
        TypeRef tr;
        tr.tag = Tag::BUILTIN;
        tr.builtin = t;
        tr.funcSig.reset();
        return tr;
    }
    
    static TypeRef functionType(std::shared_ptr<FunctionSig> sig) {
        TypeRef tr;
        tr.tag = Tag::FUNCTION;
        tr.funcSig = std::move(sig);
        return tr;
    }

    static bool isUnsigned(const TypeRef& t) {
        return t.isBuiltin() && t.builtin.isUnsigned();
    }

    bool isBuiltin() const { return tag == Tag::BUILTIN; }
    bool isFunction() const { return tag == Tag::FUNCTION; }
};

struct FunctionSig {
    std::vector<TypeRef> paramTypes;
    std::shared_ptr<TypeRef> returnType;
};

struct FunctionValue {
    std::string name;
    std::vector<std::string> paramNames;
    std::shared_ptr<FunctionSig> sig;
};

struct Value {
    TypeRef type;
    std::variant<int64_t, uint64_t, std::monostate, FunctionValue> v;
    bool isUntypedInt = false;
};

inline bool isNumeric(const Value& val) {
    return std::holds_alternative<int64_t>(val.v) || std::holds_alternative<uint64_t>(val.v);
}

inline std::variant<int64_t,uint64_t> asNum2(const Value& v) {
    if (std::holds_alternative<int64_t>(v.v)) return std::variant<int64_t,uint64_t>(std::get<int64_t>(v.v));
    if (std::holds_alternative<uint64_t>(v.v)) return std::variant<int64_t,uint64_t>(std::get<uint64_t>(v.v));
    throw std::runtime_error("non-numeric value cannot be used as a number");
}

inline const char* builtinTypeName(const Type& t) {
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

inline std::string typeName(const TypeRef& t) {
    if (t.isBuiltin()) return builtinTypeName(t.builtin);
    // Function型の表示 "(T1, T2) -> R"
    std::ostringstream oss;
    oss << "(";
    for (size_t i = 0; i < t.funcSig->paramTypes.size(); ++i) {
        if (i) oss << ", ";
        oss << typeName(t.funcSig->paramTypes[i]);
    }
    oss << ") -> " << typeName(*t.funcSig->returnType);
    return oss.str();
}

inline bool isUnsigned(const TypeRef& t) {
    return t.isBuiltin() && t.builtin.isUnsigned();
}

inline bool fits(const TypeRef& t, const std::variant<int64_t,uint64_t>& v) {
    if (!t.isBuiltin())
        throw std::runtime_error("numeric property requested on non-numeric type: " + typeName(t));
    return t.builtin.fits(v);
}