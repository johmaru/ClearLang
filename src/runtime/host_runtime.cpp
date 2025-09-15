#include <atomic>
#include <cstdint>

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
#endif