cmake_minimum_required(VERSION 3.7.1)

project(Game)

include(CTest)
include(../catch/catch.cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

list(APPEND GAME_FILES main.cpp)

set (appName "win32")
add_executable(${appName} ${GAME_FILES})
target_include_directories(${appName} PRIVATE ..)
set_property(TARGET ${appName} PROPERTY CXX_STANDARD 17)
set_property(TARGET ${appName} PROPERTY CXX_STANDARD_REQUIRED ON)
set_target_properties(${appName} PROPERTIES LINK_FLAGS /SUBSYSTEM:WINDOWS)