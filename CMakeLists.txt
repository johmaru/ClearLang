cmake_minimum_required(VERSION 3.15)
project(ClearLanguage CXX)

set(CMAKE_NO_SYSTEM_FROM_IMPORTED ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if (MSVC)
    add_compile_options(/utf-8 /EHsc)
    include(CheckCXXCompilerFlag)
    check_cxx_compiler_flag("/external:anglebrackets" MSVC_HAS_EXTERNAL_ANGLEBRACKETS)
endif()

find_package(LLVM CONFIG REQUIRED)
find_package(LLD CONFIG QUIET)

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(HandleLLVMOptions)
add_definitions(${LLVM_DEFINITIONS})

find_package(antlr4 CONFIG QUIET)
if(NOT TARGET antlr4_static AND NOT TARGET antlr4_shared)
    find_package(antlr4-runtime CONFIG QUIET)
endif()
if(NOT TARGET antlr4_static AND NOT TARGET antlr4_shared
   AND NOT TARGET antlr4-runtime AND NOT TARGET antlr4_runtime)
    message(FATAL_ERROR "antlr4 runtime package not found. Ensure vcpkg manifest is active and vcpkg.json has 'antlr4'.")
endif()

set(ANTLR4_JAR_PATH ${CMAKE_SOURCE_DIR}/tools/antlr4.jar)
set(ANTLR4_GRAMMAR_FILE ${CMAKE_SOURCE_DIR}/grammar/ClearLanguage.g4)
set(ANTLR4_OUTPUT_DIR ${CMAKE_BINARY_DIR}/generated)

file(MAKE_DIRECTORY ${ANTLR4_OUTPUT_DIR})

set(ANTLR4_GENERATED_SOURCES
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageLexer.cpp
    ${ANTLR4_OUTPUT_DIR}/ClearLanguageParser.cpp
)
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
    COMMAND ${CMAKE_COMMAND} -E make_directory ${ANTLR4_OUTPUT_DIR}
    COMMAND java -jar ${ANTLR4_JAR_PATH} -Dlanguage=Cpp -visitor -o ${ANTLR4_OUTPUT_DIR} ${ANTLR4_GRAMMAR_FILE}
    DEPENDS ${ANTLR4_GRAMMAR_FILE}
    BYPRODUCTS ${ANTLR4_GENERATED_HEADERS}
    COMMENT "Generating ANTLR parser for ClearLanguage"
)

add_custom_target(generate_parser ALL
    DEPENDS ${ANTLR4_GENERATED_SOURCES}
)

add_executable(Clear
    src/main.cpp
    src/sema/SemaBuilder.cpp
    src/ir/IRGenFromSema.cpp
    src/llvm/Executer.cpp
    src/commands/BuildCommand.cpp
    ${ANTLR4_GENERATED_SOURCES}
)

if (LLD_FOUND)
    target_link_libraries(Clear PRIVATE lldCommon lldCOFF)
    target_compile_definitions(Clear PRIVATE HAVE_LLD=1)
endif()

target_include_directories(Clear PRIVATE ${ANTLR4_OUTPUT_DIR})
target_include_directories(Clear PRIVATE ${LLVM_INCLUDE_DIRS})

llvm_map_components_to_libnames(LLVM_LIBS
  core
  support
  analysis
  transformutils
  target
  object
  irreader
  bitreader
  mc
  mcparser
  asmparser
  asmprinter

  orcjit
  executionengine
  jitlink

  native
  nativecodegen
)

target_link_libraries(Clear PRIVATE ${LLVM_LIBS})

if(TARGET antlr4_static)
    target_link_libraries(Clear PRIVATE antlr4_static)
    target_compile_definitions(Clear PRIVATE ANTLR4CPP_STATIC)
elseif(TARGET antlr4_shared)
    target_link_libraries(Clear PRIVATE antlr4_shared)
elseif(TARGET antlr4-runtime)
    target_link_libraries(Clear PRIVATE antlr4-runtime)
elseif(TARGET antlr4_runtime)
    target_link_libraries(Clear PRIVATE antlr4_runtime)
endif()

set_source_files_properties(${ANTLR4_GENERATED_HEADERS} PROPERTIES GENERATED TRUE)
target_sources(Clear PRIVATE ${ANTLR4_GENERATED_HEADERS})

add_library(clear_runtime STATIC
    src/runtime/host_runtime.cpp
)

target_link_libraries(Clear PRIVATE clear_runtime)

target_compile_definitions(Clear PRIVATE
    CLEAR_RUNTIME_LIB_DIR="$<TARGET_FILE_DIR:clear_runtime>"
    CLEAR_RUNTIME_LIB_NAME="$<TARGET_FILE_NAME:clear_runtime>"
)

target_compile_definitions(clear_runtime PRIVATE CLEAR_RUNTIME_OMIT_EXIT=1)

add_library(clear_aot_entry STATIC
    src/runtime/aot_entry_main.cpp
)

add_dependencies(Clear clear_aot_entry)

target_compile_definitions(Clear PRIVATE
    CLEAR_AOT_ENTRY_LIB_DIR="$<TARGET_FILE_DIR:clear_aot_entry>"
    CLEAR_AOT_ENTRY_LIB_NAME="$<TARGET_FILE_NAME:clear_aot_entry>"
)

if (CMAKE_EXPORT_COMPILE_COMMANDS)
  add_custom_target(copy_compile_commands ALL
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${CMAKE_BINARY_DIR}/compile_commands.json"
      "${CMAKE_SOURCE_DIR}/compile_commands.json"
    COMMENT "Copying compile_commands.json to source dir for clangd"
  )
endif()