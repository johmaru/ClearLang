#include <variant>
#define CLTYPE_H
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string>

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

struct Value {
    Type type;
    std::variant<int64_t, uint64_t, std::monostate> v;
    bool isUntypedInt = false;
};

inline bool isNumeric(const Value& val) {
    return !std::holds_alternative<std::monostate>(val.v);
}

inline std::variant<int64_t,uint64_t> asNum2(const Value& v) {
    if (std::holds_alternative<int64_t>(v.v)) return std::variant<int64_t,uint64_t>(std::get<int64_t>(v.v));
    if (std::holds_alternative<uint64_t>(v.v)) return std::variant<int64_t,uint64_t>(std::get<uint64_t>(v.v));
    throw std::runtime_error("unit value cannot be used as a number");
}