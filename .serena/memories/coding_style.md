# コードスタイルと規約

## C++ コーディング規約
- **標準**: C++17準拠
- **文字エンコーディング**: UTF-8 (MSVC: /utf-8フラグ)
- **命名規則**: 明確で一貫性のある命名
- **可読性**: 高い可読性とメンテナンス性を重視
- **冗長性**: 不要な冗長性を排除

## ファイル構成規則
- **文法ファイル**: `/grammar/ClearLanguage.g4`
- **ソースファイル**: `/src/*.cpp`, `/src/*.h`
- **生成ファイル**: `/build/generated/` (自動生成、コミット不要)

## ビルド設定
- **CMake**: compile_commands.json生成 (clangd対応)
- **ANTLR**: 静的リンク (ANTLR4CPP_STATIC定義)
- **LLVM**: 必要なコンポーネントのみリンク

## 依存関係管理
- **Conan**: antlr4-cppruntime, llvm-core
- **バージョン管理**: conanfile.txtで固定バージョン指定