# �ėp Ninja �p Conan toolchain wrapper

get_filename_component(_WRAP_ROOT "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)

if(NOT CMAKE_BUILD_TYPE)
  # VS Code / �ꕔ�c�[���ŋ�̉\��
  set(CMAKE_BUILD_TYPE "Debug")
endif()

string(TOLOWER "${CMAKE_BUILD_TYPE}" _bt_lower)

set(_CONAN_TC "${_WRAP_ROOT}/out/conan/ninja-${_bt_lower}/build/generators/conan_toolchain.cmake")

if(NOT EXISTS "${_CONAN_TC}")
  message(FATAL_ERROR
    "Conan toolchain ��������܂���: ${_CONAN_TC}\n"
    "��Ɏ��s���Ă�������:\n"
    "  conan install . -s build_type=${CMAKE_BUILD_TYPE} -of out/conan/ninja-${_bt_lower} -b missing\n")
endif()

include("${_CONAN_TC}")

# Ninja ���� VS �p platform/toolset ���������Ă����珜��
if(CMAKE_GENERATOR MATCHES "Ninja")
  foreach(var IN ITEMS CMAKE_GENERATOR_PLATFORM CMAKE_GENERATOR_TOOLSET)
    if(DEFINED ${var} AND NOT "${${var}}" STREQUAL "")
      message(STATUS "Clearing ${var}='${${var}}' for Ninja")
      unset(${var} CACHE)
      set(${var} "" CACHE STRING "" FORCE)
    endif()
  endforeach()
endif()

# 32bit ��\���������u���b�N�i�O�̂��߁j
if (MSVC AND CMAKE_SIZEOF_VOID_P EQUAL 4)
  message(FATAL_ERROR "32bit (x86) ���ō\������܂����Bx64 vcvars64 ��K�p���čč\�����Ă��������B")
endif()