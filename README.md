## ClearLanguage - Getting Start

1. Create some empty directory
2. ```Clear init```
3. ```Clear build```

## ClearLanguage - Build Guide (Windows, PowerShell)

This project uses Conan (deps), CMake (build), ANTLR (parser gen), and LLVM.

### Prerequisites
- Visual Studio 2022 with "Desktop development with C++"
- Python + Conan 2 (conan --version should work)
- Java 11+ (for ANTLR codegen): `java -version`
- Optional: Ninja (for compile_commands.json) `winget install Ninja-build.Ninja`


### Programmng Language Reference

See [language reference](./sema_readme/language_sema_en.md).

---

## Need edit .conan2 global.conf
```powershell
tools.cmake.cmaketoolchain:generator=Ninja
tools.cmake.cmaketoolchain:user_presets=
```

## Conan Profile
```powershell
[settings]
arch=x86_64
build_type=Release
compiler=msvc
compiler.cppstd=17
compiler.runtime=dynamic
compiler.version=194
os=Windows
```

## 1) Fetch dependencies (Conan)
```powershell
conan install . -s build_type=Debug -of out/conan/ninja-debug -b missing
```

This generates CMake files under `out/conan/build/generators/` and a toolchain file used by CMake.

---

## 2) Build with Visual Studio generator (recommended for day-to-day)

Debug
```powershell
conan install . -s build_type=Debug -of out/conan/ninja-debug -b missing -c tools.cmake.cmaketoolchain:generator=Ninja
cmake --build --preset ninja-debug --target generate_parser
cmake --preset ninja-debug
cmake --build --preset ninja-debug
```

Release
```powershell
conan install . -s build_type=Release -of out/conan/ninja-release -b missing
cmake --build --preset ninja-debug --target generate_parser
cmake --preset ninja-release
cmake --build --preset ninja-release
```

Build and run command is ``` Clear build --debug ```, If you have any questions in the build file, please [refer to the example](./example/).

Notes
- ANTLR codegen runs automatically (custom target `generate_parser` and dependency from `Clear`).
- Visual Studio generator does NOT produce `compile_commands.json`.

---

## 3) Generate compile_commands.json for clangd (via Ninja)
Use Ninja once to produce `compile_commands.json` and generated headers, then continue using VS if you like.

1. Clean previous Ninja build dir (if any)
```powershell
Remove-Item -Recurse -Force build-ninja -ErrorAction SilentlyContinue
```

2. Generate Conan toolchain for Ninja and configure CMake
```powershell
conan install . -of . -s build_type=Release --build=missing -c tools.cmake.cmaketoolchain:generator=Ninja
cmake -S . -B build-ninja -G Ninja -DCMAKE_TOOLCHAIN_FILE="$PWD/build/Release/generators/conan_toolchain.cmake" -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl
```

3. Run ANTLR codegen and copy `compile_commands.json` to repo root
```powershell
cmake --build --preset ninja-debug --target generate_parser
cmake --build --preset ninja-debug --target copy_compile_commands
```

After this, clangd will see generated headers under `build-ninja/generated` and flags from `compile_commands.json` in the repo root.

Troubleshooting for Ninja
- Error: "Ninja does not support platform specification, but platform x64 was specified"
	- Cause: Environment injects a Platform/architecture. Fix by clearing variables before configuring Ninja:
	```powershell
	Remove-Item Env:Platform -ErrorAction SilentlyContinue
	Remove-Item Env:CMAKE_GENERATOR_PLATFORM -ErrorAction SilentlyContinue
	Remove-Item Env:VSCMD_ARG_TGT_ARCH -ErrorAction SilentlyContinue
	```
	- If still persists, delete `build-ninja` and re-run step 2.

---

## Common issues
- Could not find package "antlr4-runtime"
	- Run Conan step first and use the toolchain when configuring CMake:
	```powershell
	conan install . -of . -s build_type=Release --build=missing
	cmake -S . -B build -G "Visual Studio 17 2022" -DCMAKE_TOOLCHAIN_FILE="$PWD/build/generators/conan_toolchain.cmake"
	```

- ANTLR headers not found (e.g., ClearLanguageLexer.h)
	- Ensure Java is installed and run the ANTLR generation (it runs via the build):
	```powershell
	cmake --build build --config Release
	```
	- For clangd usage, complete the Ninja section to generate headers and `compile_commands.json`.

- compile_commands.json doesn’t appear
	- Visual Studio generator doesn’t emit it. Use the Ninja steps above, then `copy_compile_commands` target will place it at the repo root.

-  Error copying file
   - Set an target `Clear`
   ```powershell
   cmake --build build --config Release --target Clear
   ```

- you had an x64 error for building conan for the command
   - Edit the path to the conan profile file ``` C:\Users\{Your Name}\.conan2\profiles\default ```
   ``` [settings]
	   arch=x86_64
	   build_type=Release
	   compiler=msvc
	   compiler.cppstd=17
	   compiler.runtime=dynamic
	   compiler.version=194
	   os=Windows
   ```


---

## Optional: CMake Presets
Presets exist in the repo for convenience. Example (Visual Studio):
```powershell
cmake --preset msvc-vs-release
cmake --build --preset msvc-vs-release
```

If using Ninja via presets, ensure you’ve cleared Platform variables as noted above.