cmake_minimum_required (VERSION 2.6)

project (la)

find_package(catch2 REQUIRED)
find_path(SYSTEM_INCLUDE_DIR catch/catch.hpp)
include_directories(${SYSTEM_INCLUDE_DIR})

add_executable(la la.cxx)
if(MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++latest")
endif(MSVC)

enable_testing()
add_test(NAME MyAwesomeTest COMMAND la)