# ClearLang Project Inventory (2025-08-26)

## Overview
- Purpose: A clear, statically-typed, compiled language with a simple syntax. Frontend: ANTLR; Backend: LLVM.
- Tooling: CMake + (VS/MSBuild and Ninja presets), Conan for deps, ANTLR4 Java tool for grammar to C++.

## Key Paths
- Grammar: grammar/ClearLanguage.g4
- Source: src/main.cpp (currently prints "Hello, Compiler!" and includes ClearLanguageLexer.h)
- ANTLR tool: tools/antlr4.jar
- Build roots:
  - build/ (Visual Studio generator outputs, includes ClearLanguage.sln and generated ANTLR C++ sources under build/generated)
  - build-ninja/ (Ninja generator outputs, separate compile_commands.json and generated/)
- CMake/Conan config: CMakeLists.txt, CMakePresets.json, CMakeUserPresets.json, conanfile.txt, cmake/ninja_no_platform_toolchain.cmake

## Generated Artifacts (examples)
- build/generated/: ClearLanguageLexer.* ClearLanguageParser.* ClearLanguage(Base){Listener,Visitor}.* and token/interp files
- Solutions/Projects: build/ClearLanguage.sln, targets like generate_parser, intrinsics_gen, omp_gen, acc_gen

## Current Entry Point
- src/main.cpp: placeholder main
  - Includes: "ClearLanguageLexer.h" (from generated headers)
  - TODO: Wire up ANTLR input stream -> lexer -> token stream -> parser; plan IR/codegen with LLVM.

## Typical Workflow (conceptual)
1) Ensure dependencies via Conan and Java runtime for ANTLR.
2) Configure with a CMake preset (VS or Ninja) to trigger ANTLR code generation.
3) Build targets (generate_parser happens automatically via custom command/target).
4) Run the produced executable or unit tests (once added).

## Notes / Next Steps
- Implement a minimal driver in main to parse from stdin or a file.
- Add unit tests for lexer/parser rules around ClearLanguage.g4.
- Introduce a basic AST and semantic analysis phase.
- Plan LLVM IR emission scaffolding.

## Environment
- OS: Windows. Default shell: PowerShell 5.1.
- Compilers: MSVC via VS solution; alternative Ninja + MSVC/Clang via presets.

This summary is safe to share and can be used for future onboarding or quick recalls.
