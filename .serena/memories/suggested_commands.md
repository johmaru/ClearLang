# 推奨コマンド一覧

## 依存関係のインストール
```bash
conan install . --build=missing
```

## プロジェクトの設定
```bash
cmake -S . -B build
```

## ビルド
```bash
cmake --build build
```

## 実行
```bash
.\build\Debug\Clear.exe  # Windows Debug版
.\build\Release\Clear.exe  # Windows Release版
```

## クリーンビルド
```bash
Remove-Item -Recurse -Force build
conan install . --build=missing
cmake -S . -B build
cmake --build build
```

## 開発用コマンド
```bash
# ANTLRパーサーの手動生成 (通常は自動)
java -jar tools\antlr4.jar -Dlanguage=Cpp -visitor -o build\generated grammar\ClearLanguage.g4

# compile_commands.json の生成 (clangd用)
cmake -S . -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
```

## Windowsユーティリティコマンド
```powershell
# ディレクトリリスト
Get-ChildItem (ls)
# ファイル検索
Get-ChildItem -Recurse -Name "*.cpp"
# 文字列検索
Select-String "pattern" *.cpp
# ディレクトリ移動
Set-Location (cd)
# Git操作
git status
git add .
git commit -m "message"
```