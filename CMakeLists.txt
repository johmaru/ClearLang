cmake_minimum_required(VERSION 3.15)
project(ClearLanguage CXX)

# ここで system 扱い抑止 (find_package より前)
set(CMAKE_NO_SYSTEM_FROM_IMPORTED ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if (MSVC)
    add_compile_options(/utf-8 /EHsc)
    
    include(CheckCXXCompilerFlag)
    check_cxx_compiler_flag("/external:anglebrackets" MSVC_HAS_EXTERNAL_ANGLEBRACKETS)
    if (MSVC_HAS_EXTERNAL_ANGLEBRACKETS)
        # 必要になったらコメント解除（既定は未指定＝通常扱い）
        # add_compile_options(/external:anglebrackets)
    endif()
endif()

# 依存
find_package(antlr4-runtime CONFIG REQUIRED)
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

add_custom_command(
    OUTPUT ${ANTLR4_GENERATED_SOURCES}
    COMMAND java -jar ${ANTLR4_JAR_PATH} -Dlanguage=Cpp -visitor -o ${ANTLR4_OUTPUT_DIR} ${ANTLR4_GRAMMAR_FILE}
    DEPENDS ${ANTLR4_GRAMMAR_FILE}
    BYPRODUCTS ${ANTLR4_GENERATED_HEADERS}
    COMMENT "Generating ANTLR parser for ClearLanguage"
)

add_custom_target(
    generate_parser ALL
    DEPENDS ${ANTLR4_GENERATED_SOURCES}
)

include_directories(${ANTLR4_OUTPUT_DIR})

# -----------------------------------------------------------------------------
# 3. 実行ファイルの作成とリンク
# -----------------------------------------------------------------------------
# 実行ファイルを作成
add_executable(Clear
    src/main.cpp
    src/ir/IRGenVisitor.cpp
    src/sema/SemaBuilder.cpp
    src/ir/IRGenFromSema.cpp
    src/runtime/host_runtime.cpp
    src/llvm/Executer.cpp
    ${ANTLR4_GENERATED_SOURCES}
 )

# 実行ファイルがコード生成完了を待つように依存関係を設定
add_dependencies(Clear generate_parser)

# ライブラリをリンク
target_link_libraries(Clear PRIVATE
    antlr4_static
    llvm-core::llvm-core
)

# Conan の imported からインクルードを通常 /I として再注入
foreach(_dep antlr4_static llvm-core::llvm-core)
    get_target_property(_incs ${_dep} INTERFACE_INCLUDE_DIRECTORIES)
    if(_incs)
        target_include_directories(Clear PRIVATE ${_incs})
    endif()
endforeach()

target_include_directories(Clear PRIVATE ${ANTLR4_OUTPUT_DIR})
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