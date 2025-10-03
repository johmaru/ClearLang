#pragma once
#include "CLType.h"
#include <cmath>
#include <variant>

namespace sema_utils {

inline void normalize_value_storage(value& val) {
    const auto& t = val.type;
    auto unsigned_kind = [](const type& bt){
        return bt.kind == type::kind_enum::u8 || bt.kind == type::kind_enum::u16 || bt.kind == type::kind_enum::u32 || bt.kind == type::kind_enum::u64;
    };
    if (t.is_builtin() && unsigned_kind(t.builtin)) {
        if (std::holds_alternative<int64_t>(val.v))
            val.v = static_cast<uint64_t>(std::get<int64_t>(val.v));
    } else {
        if (std::holds_alternative<uint64_t>(val.v))
            val.v = static_cast<int64_t>(std::get<uint64_t>(val.v));
    }
}

inline value coerce_untyped_int_to(const value& in, const type_ref& target) {
    if (!target.is_builtin()) throw std::runtime_error("cannot coerce to non-builtin");
    if (target.builtin.kind == type::kind_enum::unit || target.builtin.kind == type::kind_enum::noreturn)
        throw std::runtime_error("cannot coerce to unit/noreturn");

    value out{target, std::monostate{}, false};
    if (target.builtin.is_unsigned()) {
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
    auto nv = as_num2(out);
    if (!fits(target, nv)) {
        std::string msg = "initializer out of range for type ";
        msg += type_name(target);
        throw std::runtime_error(msg);
    }
    return out;
}

} // namespace sema_utils