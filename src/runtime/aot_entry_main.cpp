
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <string>
#ifdef _WIN32
#include <Windows.h>
#endif

using std::fprintf;

extern "C" {
// NOLINTBEGIN(bugprone-reserved-identifier, readability-identifier-naming)
int32_t __cl_entry(uint8_t* buf, int32_t len);
int32_t __cl_exit_code();
}

static std::filesystem::path exeDirectory() {
#ifdef _WIN32
    char buf[MAX_PATH];
    DWORD len = GetModuleFileNameA(nullptr, buf, MAX_PATH);
    if (len == 0) {
        return {};
    }
    std::filesystem::path path(buf);
    return path.parent_path();
#else
    return std::filesystem::current_path();
#endif
}

int main(int argc, char** argv) {
    bool emitIr = false;
    for (int i = 1; i < argc; ++i) {
        if (std::strcmp(argv[i], "--emit-llvm") == 0) {
            emitIr = true;
            break;
        }
    }

    if (emitIr) {
        std::filesystem::path exePath;
#ifdef _WIN32
        {
            char buf[MAX_PATH];
            DWORD len = GetModuleFileNameA(nullptr, buf, MAX_PATH);
            if (len != 0) {
                exePath = std::filesystem::path(buf).filename();
            }
        }
#else
        exePath = std::filesystem::path(argv[0]).filename();
#endif
        std::string stem = exePath.stem().string(); // "ClearApp"
        std::filesystem::path irPath = exeDirectory() / (stem + ".ll");

        FILE* file = std::fopen(irPath.string().c_str(), "rb");
        if (file == nullptr) {
            fprintf(stderr, "IR file not found: %s\n", irPath.string().c_str());
            return 1;
        }
        std::fseek(file, 0, SEEK_END);
        long streamSize = std::ftell(file);
        std::fseek(file, 0, SEEK_SET);
        std::string buf;
        buf.resize(static_cast<size_t>(streamSize));
        if (streamSize > 0) {
            size_t r = std::fread(buf.data(), 1, static_cast<size_t>(streamSize), file);
            buf.resize(r);
        }
        std::fclose(file);
        std::fwrite(buf.data(), 1, buf.size(), stdout);
        std::fflush(stdout);
        return 0;
    }

    uint8_t buf[16] = {0};
    (void) __cl_entry(buf, static_cast<int32_t>(sizeof buf));
    return __cl_exit_code();
}
// NOLINTEND(bugprone-reserved-identifier, readability-identifier-naming)