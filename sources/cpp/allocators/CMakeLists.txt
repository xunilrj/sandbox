
cmake_minimum_required(VERSION 2.8.12)

set(CMAKE_C_COMPILER "gcc")
set(CMAKE_CXX_COMPILER "g++")
SET(CMAKE_CXX_FLAGS "-g -O0 -Wall -fprofile-arcs -ftest-coverage")
SET(CMAKE_C_FLAGS "-g -O0 -Wall -W -fprofile-arcs -ftest-coverage")
SET(CMAKE_EXE_LINKER_FLAGS "-fprofile-arcs -ftest-coverage")
enable_testing()

project(allocators)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

add_executable(allocators main.cxx)
target_compile_features(allocators PRIVATE cxx_auto_type)
target_link_libraries(allocators ${CONAN_LIBS})

add_test(allocators ./bin/allocators)
