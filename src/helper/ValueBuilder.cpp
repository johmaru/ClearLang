#include "ValueBuilder.h"
#include <stdexcept>

Value ValueBuilder::makeF16FromFloat(float f) {
    if (f < -65504.0f || f > 65504.0f) {
        throw std::runtime_error("float value out of range for f16");
    }

    CLHalf h(f);

    Value val;
    val.type = TypeRef::builtinType(Type{Type::F16});
    val.v = h;
    return val;
}