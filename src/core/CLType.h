#pragma once
#include <variant>
#define CLTYPE_H
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string>
#include <memory>
#include <vector>
#include <sstream>

#if __has_include(<stdfloat>)
    #include <stdfloat>
    #define CL_HAVE_STD_FLOAT16 1
#elif defined(__FLT16_MANT_DIG__) || defined(__STD_FLOAT16_T__) || defined(__MSC_VER)

    #if defined(__clang__) || defined(__GNUC__) || defined(__MSC_VER)
        #define CL_HAVE_STD_FLOAT16 1
    #endif
#endif

struct CLHalf {
    uint16_t bits {0};
    CLHalf() = default;
    explicit CLHalf(float f) { bits = floatToHalfBits(f); }
    operator float() const { return halfBitsToFloat(bits); }

    static uint16_t floatToHalfBits(float f);
    static float halfBitsToFloat(uint16_t h);
};

inline uint16_t CLHalf::floatToHalfBits(float f) {
    union {
        float f;
        uint32_t u;
    } v{f};
    uint32_t sign = (v.u >> 16) & 0x8000u;
    uint32_t mant = v.u & 0x007FFFFFu;
    int32_t exp = int32_t((v.u >> 23) & 0xFF) - 112;

    if (exp <= 0) {
        if (exp < -10) return uint16_t(sign);
        mant |= 0x00800000u;
        uint32_t shifted = mant >> (1 - exp);
        return uint16_t(sign | (shifted + 0x00001000u) >> 13);
    } else if (exp >= 31) {
        if (mant == 0) return uint16_t(sign | 0x7C00u);
        return uint16_t(sign | 0x7C00u | (mant >> 13));
    }
    uint16_t half = uint16_t(sign | (exp << 10) | (mant + 0x00001000u) >> 13);
    return half;
}

inline float CLHalf::halfBitsToFloat(uint16_t h) {
    union {uint32_t u; float f;} v;
    uint32_t sign = (h & 0x8000u) << 16;
    uint32_t exp = (h >> 10) & 0x1Fu;
    uint32_t mant = h & 0x3FFu;
    if (exp == 0) {
        if (mant == 0) {
            v.u = sign;
        } else {
            exp = 1;
            while ((mant & 0x400u) == 0) {
                mant <<= 1;
                exp--;
            }
            mant &= 0x3FFu;
            v.u = sign | ((exp - 15 + 127) << 23) | (mant << 13);
        }
    } else if (exp == 31) {
        v.u = sign | 0x7F800000u | (mant << 13);
    } else {
        v.u = sign | ((exp - 15 + 127) << 23) | (mant << 13);
    }
    return v.f;
}

struct Type {
    enum Kind { I8, U8, I16, U16, I32, U32, I64, U64, F16, NORETURN, UNIT, STRING } Kind;
    bool isUnsigned() const {
        return Kind == U8 || Kind == U16 || Kind == U32 || Kind == U64;
    }

    std::pair<int64_t, int64_t> signedBounds() const {
        switch (Kind) {
            case I8: return {std::numeric_limits<int8_t>::min(), std::numeric_limits<int8_t>::max()};
            case I16: return {std::numeric_limits<int16_t>::min(), std::numeric_limits<int16_t>::max()};
            case I32: return {std::numeric_limits<int32_t>::min(), std::numeric_limits<int32_t>::max()};
            case I64: return {std::numeric_limits<int64_t>::min(), std::numeric_limits<int64_t>::max()};
	        case U8:
	        case U16:
	        case U32:
	        case U64:
	        case F16:
	        case NORETURN:
	        case UNIT:
	        case STRING:
				throw std::runtime_error("type is not signed");
        }
		throw std::runtime_error("unreachable");
    }

    std::pair<uint64_t, uint64_t> unsignedBounds() const {
        switch (Kind) {
            case U8: return {std::numeric_limits<uint8_t>::min(), std::numeric_limits<uint8_t>::max()};
            case U16: return {std::numeric_limits<uint16_t>::min(), std::numeric_limits<uint16_t>::max()};
            case U32: return {std::numeric_limits<uint32_t>::min(), std::numeric_limits<uint32_t>::max()};
            case U64: return {std::numeric_limits<uint64_t>::min(), std::numeric_limits<uint64_t>::max()};
	        case I8:
	        case I16:
	        case I32:
	        case I64:
	        case F16:
	        case NORETURN:
	        case UNIT:
	        case STRING:
				throw std::runtime_error("type is not unsigned");
        }
		throw std::runtime_error("unreachable");
    }

    std::pair<float, float> floatBounds() const {
        switch (Kind) {
            case F16: {
                #if defined(CL_HAVE_STD_FLOAT16)
                    return { -65504.0f, 65504.0f };
                #else
                    return { -65504.0f, 65504.0f };
                #endif
            }
        case I8:
        case U8:
        case I16:
        case U16:
        case I32:
        case U32:
        case I64:
        case U64:
        case NORETURN:
        case UNIT:
        case STRING:
			 throw std::runtime_error("type is not float");
        }
		throw std::runtime_error("unreachable");
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
        if (s == "i8") return {I8};
        if (s == "u8") return {U8};
        if (s == "i16") return {I16};
        if (s == "u16") return {U16};
        if (s == "i32" ||s == "int") return {I32};
        if (s == "u32") return {U32};
        if (s == "i64") return {I64};
        if (s == "u64") return {U64};
        if (s == "f16") return {F16};
        if (s == "noreturn") return {NORETURN};
        if (s == "unit" || s == "()") return {UNIT};
		if (s == "string") return { STRING };
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
    std::variant<int64_t, uint64_t, std::monostate, FunctionValue, CLHalf, std::string> v;
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

inline float asFloat(const Value& v) {
    if (std::holds_alternative<CLHalf>(v.v)) return static_cast<float>(std::get<CLHalf>(v.v));
    throw std::runtime_error("non-float value cannot be used as a float");
}

inline const char* builtinTypeName(const Type& t) {
    switch (t.Kind) {
        case Type::I8: return "i8";
        case Type::U8: return "u8";
        case Type::I16: return "i16";
        case Type::U16: return "u16";
        case Type::I32: return "i32";
        case Type::U32: return "u32";
        case Type::I64: return "i64";
        case Type::U64: return "u64";
        case Type::F16: return "f16";
        case Type::NORETURN: return "noreturn";
        case Type::UNIT: return "unit";
		case Type::STRING: return "string";
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

inline bool is_string(const TypeRef& t) {
    return t.isBuiltin() && t.builtin.Kind == Type::STRING;
}

inline bool is_unit(const TypeRef& t) {
    return t.isBuiltin() && t.builtin.Kind == Type::UNIT;
}

inline const std::string& as_string(const Value& v) {
    if (!std::holds_alternative<std::string>(v.v))
		throw std::runtime_error("value is not a string");
	return std::get<std::string>(v.v);
}

inline Value make_string(std::string s)
{
	return Value{ TypeRef::builtinType(Type{Type::STRING}), std::move(s), false };
}

inline Value concat_string(const Value& lhs, const Value& rhs)
{
    if (!is_string(lhs.type) || !is_string(rhs.type))
        throw std::runtime_error("type mismatch: string concat requires string + string");
    const auto& a = std::get<std::string>(lhs.v);
    const auto& b = std::get<std::string>(rhs.v);
    return make_string(a + b);
}