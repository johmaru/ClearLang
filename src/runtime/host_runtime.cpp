#include <atomic>
#include <cstdint>
#include <cstring>

static std::atomic<int32_t> g_exitCode{0};

extern "C" void __cl_set_exit_code(int32_t c){ 
    g_exitCode.store(c, std::memory_order_relaxed);
}

extern "C" int32_t __cl_exit_code(){ 
    return g_exitCode.load(std::memory_order_relaxed); 
}

#ifdef _WIN32
#include <windows.h>
extern "C" void __cl_set_exit_code_from_win32_last_error() {
    DWORD err = GetLastError();
    g_exitCode.store(static_cast<int32_t>(err), std::memory_order_relaxed);
}


extern "C" __declspec(dllexport) void __cl_printf(const char* s)
{
	if (!s) return;
    DWORD written = 0;
    const DWORD len = static_cast<DWORD>(std::strlen(s));
	HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
	if (!h || h == INVALID_HANDLE_VALUE) return;
	(void)WriteFile(h, s, len, &written, nullptr);
}
#else
#include <unistd.h>
extern "C" void __cl_printf(const char* s) {
	if (!s) return;
    size_t len = std::strlen(s);
    size_t written = write(STDOUT_FILENO, s, len);
	(void)written;
}
#endif