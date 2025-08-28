# CMakeの最低バージョンを指定
cmake_minimum_required(VERSION 3.15)

# プロジェクト名と言語を設定
project(ClearLanguage CXX)

# C++17を必須に設定 (ANTLRとLLVMが要求するため)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# clangd向け: コンパイルコマンドDBを生成
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# MSVCでUTF-8のソースを正しく扱う
if (MSVC)
    add_compile_options(/utf-8)
    # 追加: C++例外を有効化（/EHsc: 同期例外のみ有効化）
    add_compile_options(/EHsc)
endif()

# -----------------------------------------------------------------------------
# 1. Conan経由で依存関係を検索
# -----------------------------------------------------------------------------
# conan installで生成された設定を読み込み、find_packageがライブラリを見つけられるようにする
# ConanのCMakeDepsが生成するパッケージ名に合わせる（antlr4-runtime）
find_package(antlr4-runtime CONFIG REQUIRED)
# ConanのCMakeDepsが出力するLLVMConfig.cmakeを利用
find_package(LLVM CONFIG REQUIRED)

# -----------------------------------------------------------------------------
# 2. ANTLRのコード生成設定
# -----------------------------------------------------------------------------
# ANTLRツールのjarファイルへのパス
set(ANTLR4_JAR_PATH ${CMAKE_SOURCE_DIR}/tools/antlr4.jar)
# 文法ファイルへのパス
set(ANTLR4_GRAMMAR_FILE ${CMAKE_SOURCE_DIR}/grammar/ClearLanguage.g4)
# 生成されるファイルの出力先
set(ANTLR4_OUTPUT_DIR ${CMAKE_BINARY_DIR}/generated)

# 生成先ディレクトリを事前に作成
file(MAKE_DIRECTORY ${ANTLR4_OUTPUT_DIR})

# ANTLRが生成するファイル名をリスト化 (文法名が'ClearLanguage'の場合)
set(ANTLR4_GENERATED_SOURCES
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageLexer.cpp
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageParser.cpp
)

# 生成されるヘッダ（clangd 連携や依存関係解析のため BYPRODUCTS に明示しておく）
set(ANTLR4_GENERATED_HEADERS
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageLexer.h
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageParser.h
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageBaseVisitor.h
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageVisitor.h
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageBaseListener.h
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageListener.h
)

# コードを生成するカスタムコマンドを定義
add_custom_command(
    OUTPUT ${ANTLR4_GENERATED_SOURCES}
    COMMAND java -jar ${ANTLR4_JAR_PATH} -Dlanguage=Cpp -visitor -o ${ANTLR4_OUTPUT_DIR} ${ANTLR4_GRAMMAR_FILE}
    DEPENDS ${ANTLR4_GRAMMAR_FILE}
    BYPRODUCTS ${ANTLR4_GENERATED_HEADERS}
    COMMENT "Generating ANTLR parser for ClearLanguage"
)

# 生成されたソースをまとめるカスタムターゲットを作成
add_custom_target(
    generate_parser ALL
    DEPENDS ${ANTLR4_GENERATED_SOURCES}
)

# 生成されたヘッダファイルを見つけられるようにインクルードパスを追加
include_directories(${ANTLR4_OUTPUT_DIR})

# -----------------------------------------------------------------------------
# 3. 実行ファイルの作成とリンク
# -----------------------------------------------------------------------------
# 実行ファイルを作成
add_executable(Clear
    src/main.cpp
    ${ANTLR4_GENERATED_SOURCES} # 生成されたソースもコンパイル対象に含める
)

# 実行ファイルがコード生成完了を待つように依存関係を設定
add_dependencies(Clear generate_parser)

# ライブラリをリンク
target_link_libraries(Clear PRIVATE
    antlr4_static
    llvm-core::llvm-core
)

# 追加: 例外/RTTI を明示（Clang/ GCC 用）。MSVC は /EHsc を再度明示。
target_compile_options(Clear PRIVATE
    $<$<CXX_COMPILER_ID:MSVC>:/EHsc>
    $<$<CXX_COMPILER_ID:Clang>:-fexceptions -frtti>
    $<$<CXX_COMPILER_ID:GNU>:-fexceptions -frtti>
)

# 生成物ヘッダーをターゲットのインクルードに追加（clangdにも反映されやすくする）
target_include_directories(Clear PRIVATE ${ANTLR4_OUTPUT_DIR})

# antlr4の静的リンクに合わせた定義（Conanのデータに基づく）
target_compile_definitions(Clear PRIVATE ANTLR4CPP_STATIC)

# compile_commands.json をルートへコピー（VS/Make/Ninja いずれでも、ビルド時に実行可能な専用ターゲットにする）
if (CMAKE_EXPORT_COMPILE_COMMANDS)
    add_custom_target(copy_compile_commands ALL
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${CMAKE_BINARY_DIR}/compile_commands.json"
            "${CMAKE_SOURCE_DIR}/compile_commands.json"
        COMMENT "Copying compile_commands.json to source dir for clangd"
    )
endif()

# Windows/PowerShell 環境を前提に、ビルド完了後にスクリプトを実行
set(SERENA_UPDATE_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/update_serena_snapshot.ps1")
if (EXISTS ${SERENA_UPDATE_SCRIPT})
    add_custom_command(TARGET Clear POST_BUILD
        COMMAND powershell -NoProfile -ExecutionPolicy Bypass -File "${SERENA_UPDATE_SCRIPT}"
        COMMENT "Updating Serena project snapshots"
        VERBATIM
    )
endif()