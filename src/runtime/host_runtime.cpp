#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <cerrno>
#include <cstring>
#include <limits>
#include <cstdio>

static std::atomic<int32_t> g_exit_code{0};

#ifdef _WIN32
#ifndef NOMINMAX
#define NOMINMAX 1
#endif
#include <windows.h>
#define CL_API extern "C" __declspec(dllexport)
#else
#include <unistd.h>
#define CL_API extern "C"
#endif

CL_API void __cl_set_exit_code(const int32_t c) {
    g_exit_code.store(c, std::memory_order_relaxed);
}

CL_API int32_t __cl_exit_code() {
    return g_exit_code.load(std::memory_order_relaxed);
}

#ifdef _WIN32
CL_API void __cl_set_exit_code_from_win32_last_error() {
    const DWORD err = GetLastError();
    g_exit_code.store(static_cast<int32_t>(err), std::memory_order_relaxed);
}


CL_API void __cl_printf(const char* s)
{
	if (!s) return;
    DWORD written = 0;
    const DWORD len = static_cast<DWORD>(std::strlen(s));
	const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
	if (!h || h == INVALID_HANDLE_VALUE) return;
	(void)WriteFile(h, s, len, &written, nullptr);
}

CL_API void __cl_i8_printf(const int8_t i)
{
    char buf[5];
    char* p = buf;

    int v = static_cast<int>(i);
    if (v < 0) {
        *p++ = '-';
        v = -v;
    }

    char tmp[3];
    int idx = 0;
    do {
        tmp[idx++] = static_cast<char>('0' + (v % 10));
        v /= 10;
    } while (v > 0);

    while (idx > 0) {
        *p++ = tmp[--idx];
    }

    const DWORD len = static_cast<DWORD>(p - buf);
    const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (!h || h == INVALID_HANDLE_VALUE) return;
    DWORD written = 0;
    (void)WriteFile(h, buf, len, &written, nullptr);
}

CL_API void __cl_i8_printfn(const int8_t i) {

    char buf[5];
    char* p = buf;

    int v = static_cast<int>(i);
    if (v < 0) {
        *p++ = '-';
        v = -v;
    }

    char tmp[3];
    int idx = 0;
    do {
        tmp[idx++] = static_cast<char>('0' + (v % 10));
        v /= 10;
    } while (v > 0);

    while (idx > 0) {
        *p++ = tmp[--idx];
    }

    *p++ = '\n';

    const DWORD len = static_cast<DWORD>(p - buf);
    const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (!h || h == INVALID_HANDLE_VALUE) return;
    DWORD written = 0;
    (void)WriteFile(h, buf, len, &written, nullptr);

}

CL_API void __cl_u8_printfn(uint8_t ui)
{
    char buf[4];
    char* p = buf;

    char tmp[3];
    int idx = 0;

    do {
        tmp[idx++] = static_cast<char>('0' + (ui % 10));
        ui /= 10;
    } while (ui > 0);

    while (idx > 0) {
        *p++ = tmp[--idx];
    }

    *p++ = '\n';

    const DWORD len = static_cast<DWORD>(p - buf);
    const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (!h || h == INVALID_HANDLE_VALUE) return;
    DWORD written = 0;
    (void)WriteFile(h, buf, len, &written, nullptr);
}

CL_API void __cl_i16_printfn(const int16_t i) {

    char buf[7];
    char* p = buf;

    int v = static_cast<int>(i);
    if (v < 0) {
        *p++ = '-';
        v = -v;
    }

    char tmp[6];
    int idx = 0;
    do {
        tmp[idx++] = static_cast<char>('0' + (v % 10));
        v /= 10;
    } while (v > 0);

    while (idx > 0) {
        *p++ = tmp[--idx];
    }

    *p++ = '\n';

    const DWORD len = static_cast<DWORD>(p - buf);
    const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (!h || h == INVALID_HANDLE_VALUE) return;
    DWORD written = 0;
    (void)WriteFile(h, buf, len, &written, nullptr);
}

CL_API void __cl_f16_printfn(float f) {
    char buf[64];
    const int len = std::snprintf(buf, sizeof(buf), "%.5g\n", static_cast<double>(f));
    if (len < 0) return;

    const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (!h || h == INVALID_HANDLE_VALUE) return;
    DWORD written = 0;
    (void)WriteFile(h, buf, static_cast<DWORD>(len), &written, nullptr);
}

#else
CL_API void __cl_printf(const char* s) {
	if (!s) return;
    size_t len = std::strlen(s);
    size_t written = write(STDOUT_FILENO, s, len);
	(void)written;
}

CL_API void __cl_i8_printf(int8_t i)
{
    char buf[5];
    char* p = buf;

    int v = static_cast<int>(i);
    if (v < 0) {
        *p++ = '-';
        v = -v;
    }

    char tmp[3];
    int idx = 0;
    do {
        tmp[idx++] = static_cast<char>('0' + (v % 10));
        v /= 10;
    } while (v > 0);

    while (idx > 0) {
        *p++ = tmp[--idx];
    }

    *p++ = '\n';

    const size_t len = static_cast<size_t>(p - buf);
    size_t written = write(STDOUT_FILENO, buf, len);
    (void)written;
}

CL_API void __cl_u8_printfn(uint8_t u)
{
    char buf[4];
    char* p = buf;

    char tmp[3];
    int idx = 0;
    do {
        tmp[idx++] = static_cast<char>('0' + (u % 10));
        u = static_cast<uint8_t>(u / 10);
    } while (u > 0);

    while (idx > 0) {
        *p++ = tmp[--idx];
    }
    *p++ = '\n';

    const size_t len = static_cast<size_t>(p - buf);
    size_t written = write(STDOUT_FILENO, buf, len);
    (void)written;
}
#endif

CL_API char* __cl_string_concat(const char* a, const char* b) {
    const char* sa = a ? a : "";
    const char* sb = b ? b : "";
    const size_t la = std::strlen(sa);
    const size_t lb = std::strlen(sb);
    char* out = static_cast<char*>(std::malloc(la + lb + 1));
    if (!out) {
        __cl_set_exit_code(-1);
        std::abort();
    }
    std::memcpy(out, sa, la);
    std::memcpy(out + la, sb, lb);
    out[la + lb] = '\0';
    return out;
}

[[noreturn]] static void parse_fail() {
    __cl_set_exit_code(2);
    std::abort();
}

template <typename T>
static T parse_signed(const char* s) {
    if (!s) parse_fail();
    errno = 0;
    char* end = nullptr;
    long long v = std::strtoll(s, &end, 10);
    if (errno != 0 || !end || *end != '\0') parse_fail();
    if (v < static_cast<long long>(std::numeric_limits<T>::min()) ||
        v > static_cast<long long>(std::numeric_limits<T>::max())) parse_fail();
    return static_cast<T>(v);
}

template <typename T>
static T parse_unsigned(const char* s) {
    if (!s) parse_fail();
    if (s[0] == '-') parse_fail();
    errno = 0;
    char* end = nullptr;
    unsigned long long v = std::strtoull(s, &end, 10);
    if (errno != 0 || !end || *end != '\0') parse_fail();
    if (v > static_cast<unsigned long long>(std::numeric_limits<T>::max())) parse_fail();
    return static_cast<T>(v);
}

CL_API int8_t   __cl_parse_i8(const char* s) { return parse_signed<int8_t>(s); }
CL_API uint8_t  __cl_parse_u8(const char* s) { return parse_unsigned<uint8_t>(s); }
CL_API int16_t  __cl_parse_i16(const char* s) { return parse_signed<int16_t>(s); }
CL_API uint16_t __cl_parse_u16(const char* s) { return parse_unsigned<uint16_t>(s); }
CL_API int32_t  __cl_parse_i32(const char* s) { return parse_signed<int32_t>(s); }
CL_API uint32_t __cl_parse_u32(const char* s) { return parse_unsigned<uint32_t>(s); }
CL_API int64_t  __cl_parse_i64(const char* s) { return parse_signed<int64_t>(s); }
CL_API uint64_t __cl_parse_u64(const char* s) { return parse_unsigned<uint64_t>(s); }