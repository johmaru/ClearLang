#define CLTYPE_H
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string>

struct Type {
    enum Kind { U8,I8, I32, I64 } Kind;
    static Type fromString(const std::string& s) {
        if (s == "u8") return {U8};
        if (s == "i8") return {I8};
        if (s == "i64") return {I64};
        if (s == "i32" ||s == "int") return {I32};
        throw std::runtime_error("unknown type: " + s);
    }
    int64_t min() const {
        switch (Kind) {
            case U8: return 0;
            case I8: return std::numeric_limits<int8_t>::min();
            case I32: return std::numeric_limits<int32_t>::min();
            case I64: return std::numeric_limits<int64_t>::min();
        }
        return 0;
    }
    int64_t max() const {
        switch (Kind) {
            case U8: return 255;
            case I8: return std::numeric_limits<int8_t>::max();
            case I32: return std::numeric_limits<int32_t>::max();
            case I64: return std::numeric_limits<int64_t>::max();
        }
        return 0;
    }
};

struct Value {
    Type type;
    int64_t v;
    bool isUntypedInt = false;
};