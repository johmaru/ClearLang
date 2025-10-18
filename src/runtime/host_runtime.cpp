#include <basetsd.h>

#include <atomic>
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
#define NOMINMAX 1
#endif
#include <windows.h>
#define CL_API extern "C" __declspec(dllexport)
#else
#include <unistd.h>
#define CL_API extern "C" __attribute__((visibility("default")))
#endif

CL_API void __cl_set_exit_code(const int32_t c) {
  g_exit_code.store(c, std::memory_order_relaxed);
}

#ifndef CLEAR_RUNTIME_OMIT_EXIT
extern "C" int32_t __cl_exit_code() {
  return g_exit_code.load(std::memory_order_relaxed);
}
#endif

#ifdef _WIN32
CL_API void __cl_set_exit_code_from_win32_last_error() {
  const DWORD err = GetLastError();
  g_exit_code.store(static_cast<int32_t>(err), std::memory_order_relaxed);
}

CL_API void __cl_printf(const char* s) {
  if (!s) return;
  DWORD written = 0;
  const DWORD len = static_cast<DWORD>(std::strlen(s));
  const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
  if (!h || h == INVALID_HANDLE_VALUE) return;
  (void)WriteFile(h, s, len, &written, nullptr);
}

CL_API void __cl_i8_printf(const int8_t i) {
  const std::string buf = std::to_string(i);

  const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
  if (!h || h == INVALID_HANDLE_VALUE) return;
  DWORD written = 0;
  (void)WriteFile(h, buf.c_str(), static_cast<DWORD>(buf.length()), &written,
                  nullptr);
}

CL_API void __cl_i8_printfn(const int8_t i) {
  const std::string buf = std::to_string(i).append("\n");

  const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
  if (!h || h == INVALID_HANDLE_VALUE) return;
  DWORD written = 0;
  (void)WriteFile(h, buf.c_str(), static_cast<DWORD>(buf.length()), &written,
                  nullptr);
}

CL_API void __cl_u8_printfn(uint8_t ui) {
  const std::string buf = std::to_string(ui).append("\n");

  const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
  if (!h || h == INVALID_HANDLE_VALUE) return;
  DWORD written = 0;
  (void)WriteFile(h, buf.c_str(), static_cast<DWORD>(buf.length()), &written,
                  nullptr);
}

CL_API void __cl_i16_printfn(const int16_t i) {
  const std::string buf = std::to_string(i).append("\n");

  const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
  if (!h || h == INVALID_HANDLE_VALUE) return;
  DWORD written = 0;
  (void)WriteFile(h, buf.c_str(), static_cast<DWORD>(buf.length()), &written,
                  nullptr);
}

CL_API void __cl_f16_printfn(const float f) {
  std::stringstream ss;

  ss << std::setprecision(5) << std::defaultfloat << f << "\n";

  const std::string buf = ss.str();

  const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
  if (!h || h == INVALID_HANDLE_VALUE) return;
  DWORD written = 0;
  (void)WriteFile(h, buf.c_str(), static_cast<DWORD>(buf.length()), &written,
                  nullptr);
}

CL_API void __cl_f32_printfn(const float f) {
  std::stringstream ss;

  ss << std::setprecision(5) << std::defaultfloat << f << "\n";

  const std::string buf = ss.str();

  const HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
  if (!h || h == INVALID_HANDLE_VALUE) return;
  DWORD written = 0;
  (void)WriteFile(h, buf.c_str(), static_cast<DWORD>(buf.length()), &written,
                  nullptr);
}

#else
CL_API void __cl_printf(const char* s) {
  if (!s) return;
  size_t len = std::strlen(s);
  size_t written = write(STDOUT_FILENO, s, len);
  (void)written;
}

CL_API void __cl_i8_printf(int8_t i) {
  const std::string buf = std::to_string(i);

  size_t written = write(STDOUT_FILENO, buf, buf.length());
  (void)written;
}

CL_API void __cl_u8_printfn(uint8_t u) {
  const std::string buf = std::to_string(u).append("\n");

  size_t written = write(STDOUT_FILENO, buf, buf.length());
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
  int64_t v = std::strtoll(s, &end, 10);
  if (errno != 0 || !end || *end != '\0') parse_fail();
  if (v < static_cast<int64_t>(std::numeric_limits<T>::min()) ||
      v > static_cast<int64_t>(std::numeric_limits<T>::max()))
    parse_fail();
  return static_cast<T>(v);
}

template <typename T>
static T parse_unsigned(const char* s) {
  if (!s) parse_fail();
  if (s[0] == '-') parse_fail();
  errno = 0;
  char* end = nullptr;
  uint64_t v = std::strtoull(s, &end, 10);
  if (errno != 0 || !end || *end != '\0') parse_fail();
  if (v > static_cast<uint64_t>(std::numeric_limits<T>::max())) parse_fail();
  return static_cast<T>(v);
}

CL_API int8_t __cl_parse_i8(const char* s) { return parse_signed<int8_t>(s); }
CL_API uint8_t __cl_parse_u8(const char* s) {
  return parse_unsigned<uint8_t>(s);
}
CL_API int16_t __cl_parse_i16(const char* s) {
  return parse_signed<int16_t>(s);
}
CL_API uint16_t __cl_parse_u16(const char* s) {
  return parse_unsigned<uint16_t>(s);
}
CL_API int32_t __cl_parse_i32(const char* s) {
  return parse_signed<int32_t>(s);
}
CL_API uint32_t __cl_parse_u32(const char* s) {
  return parse_unsigned<uint32_t>(s);
}
CL_API int64_t __cl_parse_i64(const char* s) {
  return parse_signed<int64_t>(s);
}
CL_API uint64_t __cl_parse_u64(const char* s) {
  return parse_unsigned<uint64_t>(s);
}