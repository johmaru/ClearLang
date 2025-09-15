#pragma once
#include "CLType.h"
#include <cmath>
#include <variant>

namespace sema_utils {

inline void checkF16Range(float f) {
    if (!std::isfinite(f) || f < -65504.0f || f > 65504.0f) {
        throw std::runtime_error("initializer out of range; allowed range: [-65504.0..65504.0] for type f16");
    }
}

inline void normalizeValueStorage(Value& val) {
    const auto& t = val.type;
    auto unsignedKind = [](const Type& bt){
        return bt.Kind == Type::U8 || bt.Kind == Type::U32 || bt.Kind == Type::U64;
    };
    if (t.isBuiltin() && unsignedKind(t.builtin)) {
        if (std::holds_alternative<int64_t>(val.v))
            val.v = static_cast<uint64_t>(std::get<int64_t>(val.v));
    } else {
        if (std::holds_alternative<uint64_t>(val.v))
            val.v = static_cast<int64_t>(std::get<uint64_t>(val.v));
    }
}

inline Value coerceUntypedIntTo(const Value& in, const TypeRef& target) {
    if (!target.isBuiltin()) throw std::runtime_error("cannot coerce to non-builtin");
    if (target.builtin.Kind == Type::UNIT || target.builtin.Kind == Type::NORETURN)
        throw std::runtime_error("cannot coerce to unit/noreturn");

    Value out{target, std::monostate{}, false};
    if (target.builtin.isUnsigned()) {
        uint64_t u = std::holds_alternative<uint64_t>(in.v)
            ? std::get<uint64_t>(in.v)
            : static_cast<uint64_t>(std::get<int64_t>(in.v));
        out.v = u;
    } else {
        int64_t s = std::holds_alternative<int64_t>(in.v)
            ? std::get<int64_t>(in.v)
            : static_cast<int64_t>(std::get<uint64_t>(in.v));
        out.v = s;
    }
    auto nv = asNum2(out);
    if (!fits(target, nv)) {
        std::string msg = "initializer out of range for type ";
        msg += typeName(target);
        throw std::runtime_error(msg);
    }
    return out;
}

} // namespace sema_utils