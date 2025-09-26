# 汎用 Ninja 用 Conan toolchain wrapper

# このファイル(cmakes/wrap_conan_toolchain.cmake)の親がリポジトリルート
get_filename_component(_WRAP_ROOT "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)

# BuildType 既定
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Debug")
endif()

# Conan install 先のルートを Preset から渡せるようにする（任意）
# 未指定なら従来の ninja-debug 既定にフォールバック
if(DEFINED CONAN_OUT_ROOT AND NOT "${CONAN_OUT_ROOT}" STREQUAL "")
  set(_conan_root "${CONAN_OUT_ROOT}")
else()
  set(_conan_root "${_WRAP_ROOT}/out/conan/ninja-debug")
endif()

# Conan の toolchain 探索（single-config と multi-config の両対応）
set(_conan_base "${_conan_root}/build")

set(_candidate1 "${_conan_base}/generators/conan_toolchain.cmake")                    # single-config
set(_candidate2 "${_conan_base}/${CMAKE_BUILD_TYPE}/generators/conan_toolchain.cmake") # multi-config

if(EXISTS "${_candidate1}")
  set(_conan_toolchain_file "${_candidate1}")
elseif(EXISTS "${_candidate2}")
  set(_conan_toolchain_file "${_candidate2}")
else()
  message(FATAL_ERROR
    "Conan toolchain が見つかりませんでした:\n"
    "  ${_candidate1}\n"
    "  ${_candidate2}\n"
    "先に実行してください:\n"
    "  conan install . -s build_type=${CMAKE_BUILD_TYPE} -of out/conan/ninja-debug -b missing\n"
    "（Ninja を明示する場合: -c tools.cmake.cmaketoolchain:generator=Ninja）"
  )
endif()

message(STATUS "Using Conan toolchain: ${_conan_toolchain_file}")
include("${_conan_toolchain_file}")

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

# 32bit 誤構成を強制ブロック
if (MSVC AND CMAKE_SIZEOF_VOID_P EQUAL 4)
  message(FATAL_ERROR "32bit (x86) 環境で構成されました。x64 vcvars64 を適用して再構成してください。")
endif()