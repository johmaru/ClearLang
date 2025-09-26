# �ėp Ninja �p Conan toolchain wrapper

# ���̃t�@�C��(cmakes/wrap_conan_toolchain.cmake)�̐e�����|�W�g�����[�g
get_filename_component(_WRAP_ROOT "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)

# BuildType ����
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Debug")
endif()

# Conan install ��̃��[�g�� Preset ����n����悤�ɂ���i�C�Ӂj
# ���w��Ȃ�]���� ninja-debug ����Ƀt�H�[���o�b�N
if(DEFINED CONAN_OUT_ROOT AND NOT "${CONAN_OUT_ROOT}" STREQUAL "")
  set(_conan_root "${CONAN_OUT_ROOT}")
else()
  set(_conan_root "${_WRAP_ROOT}/out/conan/ninja-debug")
endif()

# Conan �� toolchain �T���isingle-config �� multi-config �̗��Ή��j
set(_conan_base "${_conan_root}/build")

set(_candidate1 "${_conan_base}/generators/conan_toolchain.cmake")                    # single-config
set(_candidate2 "${_conan_base}/${CMAKE_BUILD_TYPE}/generators/conan_toolchain.cmake") # multi-config

if(EXISTS "${_candidate1}")
  set(_conan_toolchain_file "${_candidate1}")
elseif(EXISTS "${_candidate2}")
  set(_conan_toolchain_file "${_candidate2}")
else()
  message(FATAL_ERROR
    "Conan toolchain ��������܂���ł���:\n"
    "  ${_candidate1}\n"
    "  ${_candidate2}\n"
    "��Ɏ��s���Ă�������:\n"
    "  conan install . -s build_type=${CMAKE_BUILD_TYPE} -of out/conan/ninja-debug -b missing\n"
    "�iNinja �𖾎�����ꍇ: -c tools.cmake.cmaketoolchain:generator=Ninja�j"
  )
endif()

message(STATUS "Using Conan toolchain: ${_conan_toolchain_file}")
include("${_conan_toolchain_file}")

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

# 32bit ��\���������u���b�N
if (MSVC AND CMAKE_SIZEOF_VOID_P EQUAL 4)
  message(FATAL_ERROR "32bit (x86) ���ō\������܂����Bx64 vcvars64 ��K�p���čč\�����Ă��������B")
endif()