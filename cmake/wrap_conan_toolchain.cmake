# 汎用 Ninja 用 Conan toolchain wrapper

get_filename_component(_WRAP_ROOT "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)

if(NOT CMAKE_BUILD_TYPE)
  # VS Code / 一部ツールで空の可能性
  set(CMAKE_BUILD_TYPE "Debug")
endif()

string(TOLOWER "${CMAKE_BUILD_TYPE}" _bt_lower)

set(_CONAN_TC "${_WRAP_ROOT}/out/conan/ninja-${_bt_lower}/build/generators/conan_toolchain.cmake")

if(NOT EXISTS "${_CONAN_TC}")
  message(FATAL_ERROR
    "Conan toolchain が見つかりません: ${_CONAN_TC}\n"
    "先に実行してください:\n"
    "  conan install . -s build_type=${CMAKE_BUILD_TYPE} -of out/conan/ninja-${_bt_lower} -b missing\n")
endif()

include("${_CONAN_TC}")

# Ninja 時に VS 用 platform/toolset が混入していたら除去
if(CMAKE_GENERATOR MATCHES "Ninja")
  foreach(var IN ITEMS CMAKE_GENERATOR_PLATFORM CMAKE_GENERATOR_TOOLSET)
    if(DEFINED ${var} AND NOT "${${var}}" STREQUAL "")
      message(STATUS "Clearing ${var}='${${var}}' for Ninja")
      unset(${var} CACHE)
      set(${var} "" CACHE STRING "" FORCE)
    endif()
  endforeach()
endif()

# 32bit 誤構成を強制ブロック（念のため）
if (MSVC AND CMAKE_SIZEOF_VOID_P EQUAL 4)
  message(FATAL_ERROR "32bit (x86) 環境で構成されました。x64 vcvars64 を適用して再構成してください。")
endif()