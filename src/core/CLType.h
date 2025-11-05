#pragma once
#include <utility>
#include <variant>
#define CLTYPE_H
#include <cstdint>
#include <limits>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#if __has_include(<stdfloat>)
#include <stdfloat>
#define CL_HAVE_STD_FLOAT16 1
#elif defined(__FLT16_MANT_DIG__) || defined(__STD_FLOAT16_T__) || defined(__MSC_VER)

#if defined(__clang__) || defined(__GNUC__) || defined(__MSC_VER)
#define CL_HAVE_STD_FLOAT16 1
#endif
#endif

struct ClHalf {
    uint16_t bits{0};
    ClHalf() = default;
};

struct ClF32 {
    uint32_t bits{0};
    ClF32() = default;
};

struct Type {
    enum class kind_enum : std::int8_t {
        I8,
        U8,
        I16,
        U16,
        I32,
        U32,
        I64,
        U64,
        F16,
        F32,
        NORETURN,
        UNIT,
        STRING,
        BOOLEAN
    };
    kind_enum kind;

    [[nodiscard]] bool isUnsigned() const {
        return kind == kind_enum::U8 || kind == kind_enum::U16 || kind == kind_enum::U32 ||
               kind == kind_enum::U64;
    }

    [[nodiscard]] std::pair<int64_t, int64_t> signedBounds() const {
        switch (kind) {
        case kind_enum::I8:
            return {std::numeric_limits<int8_t>::min(), std::numeric_limits<int8_t>::max()};
        case kind_enum::I16:
            return {std::numeric_limits<int16_t>::min(), std::numeric_limits<int16_t>::max()};
        case kind_enum::I32:
            return {std::numeric_limits<int32_t>::min(), std::numeric_limits<int32_t>::max()};
        case kind_enum::I64:
            return {std::numeric_limits<int64_t>::min(), std::numeric_limits<int64_t>::max()};
        case kind_enum::U8:
        case kind_enum::U16:
        case kind_enum::U32:
        case kind_enum::U64:
        case kind_enum::F16:
        case kind_enum::F32:
        case kind_enum::NORETURN:
        case kind_enum::UNIT:
        case kind_enum::STRING:
        case kind_enum::BOOLEAN:
            throw std::runtime_error("type is not signed");
        }
        throw std::runtime_error("unreachable");
    }

    [[nodiscard]] std::pair<uint64_t, uint64_t> unsignedBounds() const {
        switch (kind) {
        case kind_enum::U8:
            return {std::numeric_limits<uint8_t>::min(), std::numeric_limits<uint8_t>::max()};
        case kind_enum::U16:
            return {std::numeric_limits<uint16_t>::min(), std::numeric_limits<uint16_t>::max()};
        case kind_enum::U32:
            return {std::numeric_limits<uint32_t>::min(), std::numeric_limits<uint32_t>::max()};
        case kind_enum::U64:
            return {std::numeric_limits<uint64_t>::min(), std::numeric_limits<uint64_t>::max()};
        case kind_enum::I8:
        case kind_enum::I16:
        case kind_enum::I32:
        case kind_enum::I64:
        case kind_enum::F16:
        case kind_enum::F32:
        case kind_enum::NORETURN:
        case kind_enum::UNIT:
        case kind_enum::STRING:
        case kind_enum::BOOLEAN:
            throw std::runtime_error("type is not unsigned");
        }
        throw std::runtime_error("unreachable");
    }

    [[nodiscard]] bool fits(const std::variant<int64_t, uint64_t>& v) const {
        if (std::holds_alternative<int64_t>(v)) {
            if (isUnsigned()) {
                return false;
            }
            auto [minV, maxV] = signedBounds();
            const int64_t I_VAL = std::get<int64_t>(v);
            return I_VAL >= minV && I_VAL <= maxV;
        }
        if (!isUnsigned()) {
            return false;
        }
        auto [minV, maxV] = unsignedBounds();
        const uint64_t U_VAL = std::get<uint64_t>(v);
        return U_VAL >= minV && U_VAL <= maxV;
    }

    static Type fromString(const std::string& s) {
        if (s == "i8") {
            return {kind_enum::I8};
        }
        if (s == "u8") {
            return {kind_enum::U8};
        }
        if (s == "i16") {
            return {kind_enum::I16};
        }
        if (s == "u16") {
            return {kind_enum::U16};
        }
        if (s == "i32" || s == "int") {
            return {kind_enum::I32};
        }
        if (s == "u32") {
            return {kind_enum::U32};
        }
        if (s == "i64") {
            return {kind_enum::I64};
        }
        if (s == "u64") {
            return {kind_enum::U64};
        }
        if (s == "f16") {
            return {kind_enum::F16};
        }
        if (s == "f32") {
            return {kind_enum::F32};
        }
        if (s == "noreturn") {
            return {kind_enum::NORETURN};
        }
        if (s == "unit" || s == "()") {
            return {kind_enum::UNIT};
        }
        if (s == "string") {
            return {kind_enum::STRING};
        }
        if (s == "bool") {
            return {kind_enum::BOOLEAN};
        }
        throw std::runtime_error("unknown type: " + s);
    }
};

struct FunctionSig;

struct TypeRef {
    enum class tag : std::uint8_t { BUILTIN, FUNCTION } tag = tag::BUILTIN;

    Type builtin{Type::kind_enum::I32}; // default

    std::shared_ptr<FunctionSig> func_sig;

    static TypeRef builtinType(const Type type) {
        TypeRef t_ref;
        t_ref.tag = tag::BUILTIN;
        t_ref.builtin = type;
        t_ref.func_sig.reset();
        return t_ref;
    }

    static TypeRef functionType(std::shared_ptr<FunctionSig> sig) {
        TypeRef t_ref;
        t_ref.tag = tag::FUNCTION;
        t_ref.func_sig = std::move(sig);
        return t_ref;
    }

    static bool isUnsigned(const TypeRef& t_ref) {
        return t_ref.isBuiltin() && t_ref.builtin.isUnsigned();
    }

    [[nodiscard]] bool isBuiltin() const {
        return tag == tag::BUILTIN;
    }
    [[nodiscard]] bool isFunction() const {
        return tag == tag::FUNCTION;
    }
};

struct FunctionSig {
    std::vector<TypeRef> param_types;
    std::shared_ptr<TypeRef> return_type;
};

struct FunctionValue {
    std::string name;
    std::vector<std::string> param_names;
    std::shared_ptr<FunctionSig> sig;
};

struct Value {
    TypeRef type;
    std::variant<int64_t, uint64_t, std::monostate, FunctionValue, ClHalf, std::string, ClF32> v;
    bool is_untyped_int = false;
    Value() = default;
    Value(TypeRef type,
          std::variant<int64_t, uint64_t, std::monostate, FunctionValue, ClHalf, std::string, ClF32>
              value,
          bool cond);
};

inline Value::Value(
    TypeRef type,
    std::variant<int64_t, uint64_t, std::monostate, FunctionValue, ClHalf, std::string, ClF32>
        value,
    const bool COND)
    : type(std::move(type)), v(std::move(value)), is_untyped_int(COND) {}

inline bool isNumeric(const Value& val) {
    return std::holds_alternative<int64_t>(val.v) || std::holds_alternative<uint64_t>(val.v);
}

inline std::variant<int64_t, uint64_t> asNum2(const Value& val) {
    if (std::holds_alternative<int64_t>(val.v)) {
        return std::variant<int64_t, uint64_t>(std::get<int64_t>(val.v));
    }
    if (std::holds_alternative<uint64_t>(val.v)) {
        return std::variant<int64_t, uint64_t>(std::get<uint64_t>(val.v));
    }
    throw std::runtime_error("non-numeric value cannot be used as a number");
}

inline const char* builtinTypeName(const Type& type) {
    switch (type.kind) {
    case Type::kind_enum::I8:
        return "i8";
    case Type::kind_enum::U8:
        return "u8";
    case Type::kind_enum::I16:
        return "i16";
    case Type::kind_enum::U16:
        return "u16";
    case Type::kind_enum::I32:
        return "i32";
    case Type::kind_enum::U32:
        return "u32";
    case Type::kind_enum::I64:
        return "i64";
    case Type::kind_enum::U64:
        return "u64";
    case Type::kind_enum::F16:
        return "f16";
    case Type::kind_enum::F32:
        return "f32";
    case Type::kind_enum::NORETURN:
        return "noreturn";
    case Type::kind_enum::UNIT:
        return "unit";
    case Type::kind_enum::STRING:
        return "string";
    case Type::kind_enum::BOOLEAN:
        return "bool";
    }
    return "?";
}

inline std::string typeName(const TypeRef& t_ref) {
    if (t_ref.isBuiltin()) {
        return builtinTypeName(t_ref.builtin);
    }
    // "(T1, T2) -> R"
    std::ostringstream oss;
    oss << "(";
    for (size_t i = 0; i < t_ref.func_sig->param_types.size(); ++i) {
        if (i != 0U) {
            oss << ", ";
        }
        oss << typeName(t_ref.func_sig->param_types[i]);
    }
    oss << ") -> " << typeName(*t_ref.func_sig->return_type);
    return oss.str();
}

inline bool isUnsigned(const TypeRef& t_ref) {
    return t_ref.isBuiltin() && t_ref.builtin.isUnsigned();
}

inline bool fits(const TypeRef& t_ref, const std::variant<int64_t, uint64_t>& val) {
    if (!t_ref.isBuiltin()) {
        throw std::runtime_error("numeric property requested on non-numeric type: " +
                                 typeName(t_ref));
    }
    return t_ref.builtin.fits(val);
}

inline bool isString(const TypeRef& t_ref) {
    return t_ref.isBuiltin() && t_ref.builtin.kind == Type::kind_enum::STRING;
}

inline bool isUnit(const TypeRef& t_ref) {
    return t_ref.isBuiltin() && t_ref.builtin.kind == Type::kind_enum::UNIT;
}

inline const std::string& asString(const Value& val) {
    if (!std::holds_alternative<std::string>(val.v)) {
        throw std::runtime_error("value is not a string");
    }
    return std::get<std::string>(val.v);
}

inline Value makeString(std::string str) {
    return Value{TypeRef::builtinType(Type{Type::kind_enum::STRING}), std::move(str), false};
}

inline Value concatString(const Value& lhs, const Value& rhs) {
    if (!isString(lhs.type) || !isString(rhs.type)) {
        throw std::runtime_error("type mismatch: string concat requires string + string");
    }
    const auto& a_str = std::get<std::string>(lhs.v);
    const auto& b_str = std::get<std::string>(rhs.v);
    return makeString(a_str + b_str);
}