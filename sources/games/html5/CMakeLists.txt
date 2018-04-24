# CMakeLists.txt
cmake_minimum_required(VERSION 3.0)
project(gametest)

find_package(uwebsockets  REQUIRED)

add_executable(gametest main.cpp)
