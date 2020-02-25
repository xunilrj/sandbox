cmake_minimum_required(VERSION 3.7.1)

project(linear_algebra)

include(CTest)
include(Catch.cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

list(APPEND APP_FILES main.cpp)

set (APP_NAME "linear_algebra_tests")
add_executable(${APP_NAME} ${APP_FILES})
set_property(TARGET ${APP_NAME} PROPERTY CXX_STANDARD 17)
set_property(TARGET ${APP_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_target_properties(${APP_NAME} PROPERTIES LINK_FLAGS /SUBSYSTEM:CONSOLE)

catch_discover_tests(${APP_NAME})