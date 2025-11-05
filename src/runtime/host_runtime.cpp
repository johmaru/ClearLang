#include <atomic>
#include <basetsd.h>
#include <cerrno>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <limits>
#include <sstream>

static std::atomic<int32_t> g_exit_code{0};

#ifdef _WIN32
#ifndef NOMINMAX
#define NOMINMAX constexpr 1
#endif
#include <windows.h>
#define CL_API extern "C" __declspec(dllexport)
#else
#include <unistd.h>
#define CL_API extern "C" __attribute__((visibility("default")))
#endif

// NOLINTBEGIN(bugprone-reserved-identifier, readability-identifier-naming)

CL_API void __cl_set_exit_code(const int32_t c) {
    g_exit_code.store(c, std::memory_order_relaxed);
}

#ifndef CLEAR_RUNTIME_OMIT_EXIT
CL_API int32_t __cl_exit_code() {
    return g_exit_code.load(std::memory_order_relaxed);
}
#endif

#ifdef _WIN32
CL_API void __cl_set_exit_code_from_win32_last_error() {
    const DWORD err = GetLastError();
    g_exit_code.store(static_cast<int32_t>(err), std::memory_order_relaxed);
}

CL_API void __cl_printf(const char* str) {
    if (!str) {
        return;
    }
    DWORD written = 0;
    const auto len = static_cast<DWORD>(std::strlen(str));
    const HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if ((handle == nullptr) || handle == INVALID_HANDLE_VALUE) {
        return;
    }
    (void) WriteFile(handle, str, len, &written, nullptr);
}

CL_API void __cl_i8_printf(const int8_t int_i) {
    const std::string buf = std::to_string(int_i);

    const HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (!handle || handle == INVALID_HANDLE_VALUE) {
        return;
    }
    DWORD written = 0;
    (void) WriteFile(handle, buf.c_str(), static_cast<DWORD>(buf.length()), &written, nullptr);
}

CL_API void __cl_i8_printfn(const int8_t int_i) {
    const std::string buf = std::to_string(int_i).append("\n");

    const HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if ((handle == nullptr) || handle == INVALID_HANDLE_VALUE) {
        return;
    }
    DWORD written = 0;
    (void) WriteFile(handle, buf.c_str(), static_cast<DWORD>(buf.length()), &written, nullptr);
}

CL_API void __cl_u8_printfn(uint8_t uint_i) {
    const std::string buf = std::to_string(uint_i).append("\n");

    const HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if ((handle == nullptr) || handle == INVALID_HANDLE_VALUE) {
        return;
    }
    DWORD written = 0;
    (void) WriteFile(handle, buf.c_str(), static_cast<DWORD>(buf.length()), &written, nullptr);
}

CL_API void __cl_i16_printfn(const int16_t int_i) {
    const std::string buf = std::to_string(int_i).append("\n");

    const HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if ((handle == nullptr) || handle == INVALID_HANDLE_VALUE) {
        return;
    }
    DWORD written = 0;
    (void) WriteFile(handle, buf.c_str(), static_cast<DWORD>(buf.length()), &written, nullptr);
}

CL_API void __cl_f16_printfn(const float float_f) {
    std::stringstream s_s;

    s_s << std::setprecision(5) << std::defaultfloat << float_f << "\n";

    const std::string buf = s_s.str();

    const HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if ((handle == nullptr) || handle == INVALID_HANDLE_VALUE) {
        return;
    }
    DWORD written = 0;
    (void) WriteFile(handle, buf.c_str(), static_cast<DWORD>(buf.length()), &written, nullptr);
}

CL_API void __cl_f32_printfn(const float float_f) {
    std::stringstream s_s;

    s_s << std::setprecision(5) << std::defaultfloat << float_f << "\n";

    const std::string buf = s_s.str();

    const HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if ((handle == nullptr) || handle == INVALID_HANDLE_VALUE) {
        return;
    }
    DWORD written = 0;
    (void) WriteFile(handle, buf.c_str(), static_cast<DWORD>(buf.length()), &written, nullptr);
}

#else
CL_API void __cl_printf(const char* s) {
    if (!s)
        return;
    size_t len = std::strlen(s);
    size_t written = write(STDOUT_FILENO, s, len);
    (void) written;
}

CL_API void __cl_i8_printf(int8_t i) {
    const std::string buf = std::to_string(i);

    size_t written = write(STDOUT_FILENO, buf, buf.length());
    (void) written;
}

CL_API void __cl_u8_printfn(uint8_t u) {
    const std::string buf = std::to_string(u).append("\n");

    size_t written = write(STDOUT_FILENO, buf, buf.length());
    (void) written;
}
#endif

CL_API char* __cl_string_concat(const char* a, const char* b) {
    const char* s_a = a ? a : "";
    const char* s_b = b ? b : "";
    const size_t l_a = std::strlen(s_a);
    const size_t l_b = std::strlen(s_b);
    char* out = static_cast<char*>(std::malloc(l_a + l_b + 1));
    if (out == nullptr) {
        __cl_set_exit_code(-1);
        std::abort();
    }
    std::memcpy(out, s_a, l_a);
    std::memcpy(out + l_a, s_b, l_b);
    out[l_a + l_b] = '\0';
    return out;
}

[[noreturn]] static void parse_fail() {
    __cl_set_exit_code(2);
    std::abort();
}

template <typename T> static T parse_signed(const char* str) {
    if (!str) {
        parse_fail();
    }
    errno = 0;
    char* end = nullptr;
    int64_t int64_v = std::strtoll(str, &end, 10);
    if (errno != 0 || !end || *end != '\0') {
        parse_fail();
    }
    if (int64_v < static_cast<int64_t>(std::numeric_limits<T>::min()) ||
        int64_v > static_cast<int64_t>(std::numeric_limits<T>::max())) {
        parse_fail();
    }
    return static_cast<T>(int64_v);
}

template <typename T> static T parse_unsigned(const char* str) {
    if (!str) {
        parse_fail();
    }
    if (str[0] == '-') {
        parse_fail();
    }
    errno = 0;
    char* end = nullptr;
    uint64_t uint64_v = std::strtoull(str, &end, 10);
    if (errno != 0 || !end || *end != '\0') {
        parse_fail();
    }
    if (uint64_v > static_cast<uint64_t>(std::numeric_limits<T>::max())) {
        parse_fail();
    }
    return static_cast<T>(uint64_v);
}

CL_API int8_t __cl_parse_i8(const char* str) {
    return parse_signed<int8_t>(str);
}
CL_API uint8_t __cl_parse_u8(const char* str) {
    return parse_unsigned<uint8_t>(str);
}
CL_API int16_t __cl_parse_i16(const char* str) {
    return parse_signed<int16_t>(str);
}
CL_API uint16_t __cl_parse_u16(const char* str) {
    return parse_unsigned<uint16_t>(str);
}
CL_API int32_t __cl_parse_i32(const char* str) {
    return parse_signed<int32_t>(str);
}
CL_API uint32_t __cl_parse_u32(const char* str) {
    return parse_unsigned<uint32_t>(str);
}
CL_API int64_t __cl_parse_i64(const char* str) {
    return parse_signed<int64_t>(str);
}
CL_API uint64_t __cl_parse_u64(const char* str) {
    return parse_unsigned<uint64_t>(str);
}

// NOLINTEND(bugprone-reserved-identifier, readability-identifier-naming)