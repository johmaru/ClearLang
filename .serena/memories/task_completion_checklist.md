# タスク完了時の手順

## 1. コードの検証
- ビルドエラーがないことを確認
- ANTLRパーサーが正常に生成されること
- LLVMライブラリとの連携が機能すること

## 2. 必要に応じて実行
```bash
# ビルド
cmake --build build

# 実行テスト
.\build\Debug\Clear.exe
```

## 3. コード品質チェック
- clangdによる静的解析 (compile_commands.json利用)
- コーディング規約の遵守確認
- 可読性とメンテナンス性の確認

## 4. コミット準備
```bash
# 変更確認
git status
git diff

# 必要に応じて生成ファイルを除外
# (.gitignoreに/build/が含まれることを確認)
```

## 5. 文書の更新
- 言語仕様の変更があれば文書更新
- 新機能の使用例を追加
- README.mdの更新 (必要に応じて)

## 注意事項
- `/build/generated/`の生成ファイルはコミットしない
- Conanの依存関係変更時は`conan install`を再実行
- CMakeファイル変更時は設定から再実行