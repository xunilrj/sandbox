cmake_minimum_required(VERSION 3.7.1)

project(ecscontainer)

include(CTest)
include(Catch.cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

list(APPEND APP_FILES main.cpp)

set (APP_NAME "ecscontainer.tests")
add_executable(${APP_NAME} ${APP_FILES})

catch_discover_tests(${APP_NAME})