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

struct cl_half {
    uint16_t bits {0};
    cl_half() = default;
};

struct cl_f32 {
    uint32_t bits{ 0 };
    cl_f32() = default;
};


struct type {
    enum class kind_enum : std::int8_t { i8, u8, i16, u16, i32, u32, i64, u64, f16, f32, noreturn, unit, string, boolean };
    kind_enum kind;

    [[nodiscard]] bool is_unsigned() const {
        return kind == kind_enum::u8 || kind == kind_enum::u16 || kind == kind_enum::u32 || kind == kind_enum::u64;
    }

    [[nodiscard]] std::pair<int64_t, int64_t> signed_bounds() const {
        switch (kind) {
            case kind_enum::i8: return {std::numeric_limits<int8_t>::min(), std::numeric_limits<int8_t>::max()};
            case kind_enum::i16: return {std::numeric_limits<int16_t>::min(), std::numeric_limits<int16_t>::max()};
            case kind_enum::i32: return {std::numeric_limits<int32_t>::min(), std::numeric_limits<int32_t>::max()};
            case kind_enum::i64: return {std::numeric_limits<int64_t>::min(), std::numeric_limits<int64_t>::max()};
	        case kind_enum::u8:
	        case kind_enum::u16:
	        case kind_enum::u32:
	        case kind_enum::u64:
	        case kind_enum::f16:
			case kind_enum::f32:
	        case kind_enum::noreturn:
	        case kind_enum::unit:
	        case kind_enum::string:
			case kind_enum::boolean:
				throw std::runtime_error("type is not signed");
        }
		throw std::runtime_error("unreachable");
    }

    [[nodiscard]] std::pair<uint64_t, uint64_t> unsigned_bounds() const {
        switch (kind) {
            case kind_enum::u8: return {std::numeric_limits<uint8_t>::min(), std::numeric_limits<uint8_t>::max()};
            case kind_enum::u16: return {std::numeric_limits<uint16_t>::min(), std::numeric_limits<uint16_t>::max()};
            case kind_enum::u32: return {std::numeric_limits<uint32_t>::min(), std::numeric_limits<uint32_t>::max()};
            case kind_enum::u64: return {std::numeric_limits<uint64_t>::min(), std::numeric_limits<uint64_t>::max()};
	        case kind_enum::i8:
	        case kind_enum::i16:
	        case kind_enum::i32:
	        case kind_enum::i64:
	        case kind_enum::f16:
			case kind_enum::f32:
	        case kind_enum::noreturn:
	        case kind_enum::unit:
	        case kind_enum::string:
			case kind_enum::boolean:
				throw std::runtime_error("type is not unsigned");
        }
		throw std::runtime_error("unreachable");
    }

    [[nodiscard]] bool fits(const std::variant<int64_t,uint64_t>& v) const {
        if (std::holds_alternative<int64_t>(v)) {
            if (is_unsigned()) return false;
            auto [minV, maxV] = signed_bounds();
            const int64_t iv = std::get<int64_t>(v);
            return iv >= minV && iv <= maxV;
        }
        if (!is_unsigned()) return false;
        auto [minV, maxV] = unsigned_bounds();
        const uint64_t uv = std::get<uint64_t>(v);
        return uv >= minV && uv <= maxV;
    }

    static type from_string(const std::string& s) {
        if (s == "i8") return {kind_enum::i8};
        if (s == "u8") return {kind_enum::u8};
        if (s == "i16") return {kind_enum::i16};
        if (s == "u16") return {kind_enum::u16};
        if (s == "i32" ||s == "int") return {kind_enum::i32};
        if (s == "u32") return {kind_enum::u32};
        if (s == "i64") return {kind_enum::i64};
        if (s == "u64") return {kind_enum::u64};
        if (s == "f16") return {kind_enum::f16};
        if (s == "f32") return { kind_enum::f32 };
        if (s == "noreturn") return {kind_enum::noreturn};
        if (s == "unit" || s == "()") return {kind_enum::unit};
		if (s == "string") return {kind_enum::string };
        if (s == "bool") return { kind_enum::boolean };
        throw std::runtime_error("unknown type: " + s);
    }
};

struct function_sig;

struct type_ref {
    enum class tag : std::uint8_t { builtin, function } tag = tag::builtin;

    type builtin{type::kind_enum::i32}; // default

    std::shared_ptr<function_sig> func_sig;

    static type_ref builtin_type(const type t) {
        type_ref tr;
        tr.tag = tag::builtin;
        tr.builtin = t;
        tr.func_sig.reset();
        return tr;
    }
    
    static type_ref function_type(std::shared_ptr<function_sig> sig) {
        type_ref tr;
        tr.tag = tag::function;
        tr.func_sig = std::move(sig);
        return tr;
    }

    static bool is_unsigned(const type_ref& t) {
        return t.is_builtin() && t.builtin.is_unsigned();
    }

    [[nodiscard]] bool is_builtin() const { return tag == tag::builtin; }
    [[nodiscard]] bool is_function() const { return tag == tag::function; }
};

struct function_sig {
    std::vector<type_ref> param_types;
    std::shared_ptr<type_ref> return_type;
};

struct function_value {
    std::string name;
    std::vector<std::string> param_names;
    std::shared_ptr<function_sig> sig;
};

struct value {
    type_ref type;
    std::variant<int64_t, uint64_t, std::monostate, function_value, cl_half, std::string, cl_f32> v;
    bool is_untyped_int = false;
    value() = default;
    value(const type_ref& type, std::variant<int64_t, uint64_t, std::monostate, function_value, cl_half, std::string, cl_f32> value, bool cond);
};

inline value::value(const type_ref& type,
    std::variant<int64_t, uint64_t, std::monostate, function_value, cl_half, std::string, cl_f32> value,
    const bool cond) : type(type), v(std::move(value)), is_untyped_int(cond) {}


inline bool is_numeric(const value& val) {
    return std::holds_alternative<int64_t>(val.v) || std::holds_alternative<uint64_t>(val.v);
}

inline std::variant<int64_t,uint64_t> as_num2(const value& v) {
    if (std::holds_alternative<int64_t>(v.v)) return std::variant<int64_t,uint64_t>(std::get<int64_t>(v.v));
    if (std::holds_alternative<uint64_t>(v.v)) return std::variant<int64_t,uint64_t>(std::get<uint64_t>(v.v));
    throw std::runtime_error("non-numeric value cannot be used as a number");
}

inline const char* builtin_type_name(const type& t) {
    switch (t.kind) {
        case type::kind_enum::i8: return "i8";
        case type::kind_enum::u8: return "u8";
        case type::kind_enum::i16: return "i16";
        case type::kind_enum::u16: return "u16";
        case type::kind_enum::i32: return "i32";
        case type::kind_enum::u32: return "u32";
        case type::kind_enum::i64: return "i64";
        case type::kind_enum::u64: return "u64";
        case type::kind_enum::f16: return "f16";
        case type::kind_enum::f32: return "f32";
        case type::kind_enum::noreturn: return "noreturn";
        case type::kind_enum::unit: return "unit";
		case type::kind_enum::string: return "string";
        case type::kind_enum::boolean: return "bool";
    }
    return "?";
}

inline std::string type_name(const type_ref& t) {
    if (t.is_builtin()) return builtin_type_name(t.builtin);
    // Function型の表示 "(T1, T2) -> R"
    std::ostringstream oss;
    oss << "(";
    for (size_t i = 0; i < t.func_sig->param_types.size(); ++i) {
        if (i) oss << ", ";
        oss << type_name(t.func_sig->param_types[i]);
    }
    oss << ") -> " << type_name(*t.func_sig->return_type);
    return oss.str();
}

inline bool is_unsigned(const type_ref& t) {
    return t.is_builtin() && t.builtin.is_unsigned();
}

inline bool fits(const type_ref& t, const std::variant<int64_t,uint64_t>& v) {
    if (!t.is_builtin())
        throw std::runtime_error("numeric property requested on non-numeric type: " + type_name(t));
    return t.builtin.fits(v);
}

inline bool is_string(const type_ref& t) {
    return t.is_builtin() && t.builtin.kind == type::kind_enum::string;
}

inline bool is_unit(const type_ref& t) {
    return t.is_builtin() && t.builtin.kind == type::kind_enum::unit;
}

inline const std::string& as_string(const value& v) {
    if (!std::holds_alternative<std::string>(v.v))
		throw std::runtime_error("value is not a string");
	return std::get<std::string>(v.v);
}

inline value make_string(std::string s)
{
	return value{ type_ref::builtin_type(type{type::kind_enum::string}), std::move(s), false };
}

inline value concat_string(const value& lhs, const value& rhs)
{
    if (!is_string(lhs.type) || !is_string(rhs.type))
        throw std::runtime_error("type mismatch: string concat requires string + string");
    const auto& a = std::get<std::string>(lhs.v);
    const auto& b = std::get<std::string>(rhs.v);
    return make_string(a + b);
}