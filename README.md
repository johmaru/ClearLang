## ClearLanguage - Getting Start

1. Create some empty directory
2. `Clear init`
3. `Clear build`

## ClearLanguage - Build Guide (Windows, PowerShell)

This project uses vcpkg (deps), CMake (build), ANTLR (parser gen), and LLVM.

### Prerequisites
- Visual Studio 2022 with "Desktop development with C++"
- Java 11+ (for ANTLR codegen): `java -version`
- Optional: Ninja (for compile_commands.json) `winget install Ninja-build.Ninja`
- vcpkg (Visual Studio auto-acquire in manifest mode, or manual clone+bootstrap)


### Programming Language Reference

See [language reference](./sema_readme/language_sema_en.md).

---

## 1) Dependencies via vcpkg (manifest mode)

The repo contains `vcpkg.json` (llvm with lld/tools, antlr4). Visual Studio can auto-acquire vcpkg and install dependencies when configuring CMake.

Manual setup (optional):
```powershell
git clone https://github.com/microsoft/vcpkg C:\src\vcpkg
C:\src\vcpkg\bootstrap-vcpkg.bat
```

---

## 2) Configure and Build (CMake Presets, Ninja)

Use the provided preset `ninja-debug-vcpkg` (see `CMakePresets.json`).

```powershell
# configure
cmake --preset ninja-debug-vcpkg

# build (ANTLR codegen runs automatically via generate_parser)
cmake --build --preset ninja-debug-vcpkg
```

Notes
- Generated parser sources/headers are under `out/build/ninja-debug/generated`.
- No need to call `--target generate_parser` manually; `Clear` depends on it.
- In Visual Studio, select the preset and run Configure/Build.

---

## 3) Generate compile_commands.json for clangd
Ninja builds emit `compile_commands.json` in the build dir:

```powershell
cmake --preset ninja-debug-vcpkg
cmake --build --preset ninja-debug-vcpkg
```

Point clangd to `out/build/ninja-debug/compile_commands.json` or copy it to the repo root if preferred.

Troubleshooting for Ninja
- Error: "Ninja does not support platform specification, but platform x64 was specified"
  - Clear env before configure:
  ```powershell
  Remove-Item Env:Platform -ErrorAction SilentlyContinue
  Remove-Item Env:CMAKE_GENERATOR_PLATFORM -ErrorAction SilentlyContinue
  Remove-Item Env:VSCMD_ARG_TGT_ARCH -ErrorAction SilentlyContinue
  ```

---

## Common issues

- LLVM not found (LLVMConfig.cmake)
  - Configure with vcpkg toolchain/preset. If needed, set `LLVM_DIR` to `<build>/vcpkg_installed/x64-windows/share/llvm`.

- ANTLR headers not found (e.g., ClearLanguageLexer.h)
  - Ensure Java is installed and `tools/antlr4.jar` exists.
  - The build triggers ANTLR codegen automatically via `generate_parser`.


---

## Optional: CMake Presets
Presets exist in the repo for convenience. Example (Visual Studio):
```powershell
cmake --preset msvc-vs-release
cmake --build --preset msvc-vs-release
```

If using Ninja via presets, ensure you’ve cleared Platform variables as noted above.