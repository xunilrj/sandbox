cmake_minimum_required(VERSION 3.7.1)

project(math)

include(CTest)
include(Catch.cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

list(APPEND LIB_FILES src/math.h)
list(APPEND LIB_FILES src/math.cpp)
set (LIB_NAME "math")
add_library(${LIB_NAME} ${LIB_FILES})
target_include_directories(${LIB_NAME} PRIVATE include)
set_property(TARGET ${LIB_NAME} PROPERTY CXX_STANDARD 20)
set_property(TARGET ${LIB_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)

list(APPEND TEST_FILES tests/properties.h)
list(APPEND TEST_FILES tests/main.cpp)
set (TEST_NAME "math_tests")
add_executable(${TEST_NAME} ${TEST_FILES})
target_include_directories(${TEST_NAME} PRIVATE include)
set_property(TARGET ${TEST_NAME} PROPERTY CXX_STANDARD 20)
set_property(TARGET ${TEST_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_target_properties(${TEST_NAME} PROPERTIES LINK_FLAGS /SUBSYSTEM:CONSOLE)

catch_discover_tests(${TEST_NAME})