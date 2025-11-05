#pragma once
#include "CLType.h"

#include <variant>

namespace sema_utils {

inline void normalizeValueStorage(Value& val) {
    const auto& t_ref = val.type;
    auto unsigned_kind = [](const Type& b_type) {
        return b_type.kind == Type::kind_enum::U8 || b_type.kind == Type::kind_enum::U16 ||
               b_type.kind == Type::kind_enum::U32 || b_type.kind == Type::kind_enum::U64;
    };
    if (t_ref.isBuiltin() && unsigned_kind(t_ref.builtin)) {
        if (std::holds_alternative<int64_t>(val.v)) {
            val.v = static_cast<uint64_t>(std::get<int64_t>(val.v));
        }
    } else {
        if (std::holds_alternative<uint64_t>(val.v)) {
            val.v = static_cast<int64_t>(std::get<uint64_t>(val.v));
        }
    }
}

inline Value coerceUntypedIntTo(const Value& in_val, const TypeRef& target) {
    if (!target.isBuiltin()) {
        throw std::runtime_error("cannot coerce to non-builtin");
    }
    if (target.builtin.kind == Type::kind_enum::UNIT ||
        target.builtin.kind == Type::kind_enum::NORETURN) {
        throw std::runtime_error("cannot coerce to unit/noreturn");
    }

    Value out{target, std::monostate{}, false};
    if (target.builtin.isUnsigned()) {
        uint64_t u_holds = std::holds_alternative<uint64_t>(in_val.v)
                               ? std::get<uint64_t>(in_val.v)
                               : static_cast<uint64_t>(std::get<int64_t>(in_val.v));
        out.v = u_holds;
    } else {
        int64_t i_holds = std::holds_alternative<int64_t>(in_val.v)
                              ? std::get<int64_t>(in_val.v)
                              : static_cast<int64_t>(std::get<uint64_t>(in_val.v));
        out.v = i_holds;
    }
    auto n_variant = asNum2(out);
    if (!fits(target, n_variant)) {
        std::string msg = "initializer out of range for type ";
        msg += typeName(target);
        throw std::runtime_error(msg);
    }
    return out;
}

} // namespace sema_utils