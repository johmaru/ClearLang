
#include <cstdint>

extern "C" {
// NOLINTBEGIN(bugprone-reserved-identifier, readability-identifier-naming)
int32_t __cl_entry(uint8_t* buf, int32_t len);
int32_t __cl_exit_code();
}

int main(int /* argc */, char** /* argv */) {
    uint8_t buf[16] = {0};
    (void) __cl_entry(buf, static_cast<int32_t>(sizeof buf));
    return __cl_exit_code();
}
// NOLINTEND(bugprone-reserved-identifier, readability-identifier-naming)