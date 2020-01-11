cmake_minimum_required(VERSION 3.7.1)

project(lru)

include(CTest)
include(../catch/Catch.cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

list(APPEND APP_FILES main.cpp)

set (APP_NAME "lru.tests")
add_executable(${APP_NAME} ${APP_FILES})
target_include_directories(${APP_NAME} PRIVATE ..)
set_property(TARGET ${APP_NAME} PROPERTY CXX_STANDARD 17)
set_property(TARGET ${APP_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_target_properties(${APP_NAME} PROPERTIES LINK_FLAGS /SUBSYSTEM:CONSOLE)

catch_discover_tests(${APP_NAME})