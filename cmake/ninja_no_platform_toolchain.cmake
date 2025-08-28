include_guard()

set(_CONAN_TOOLCHAIN "${CMAKE_CURRENT_LIST_DIR}/../build/generators/conan_toolchain.cmake")
if(NOT EXISTS "${_CONAN_TOOLCHAIN}")
    message(FATAL_ERROR "Conan toolchain not found at ${_CONAN_TOOLCHAIN}")
endif()

include("${_CONAN_TOOLCHAIN}")

if(CMAKE_GENERATOR MATCHES "Ninja")
    message(STATUS "Wrapper: Removing CMAKE_GENERATOR_PLATFORM for Ninja")
    unset(CMAKE_GENERATOR_PLATFORM CACHE)

    unset(CMAKE_GENERATOR_TOOLSET CACHE)
    unset(CMAKE_VS_PLATFORM_TOOLSET CACHE)

    set(CMAKE_GENERATOR_TOOLSET "" CACHE STRING "" FORCE)
endif()
