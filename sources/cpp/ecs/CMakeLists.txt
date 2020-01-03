cmake_minimum_required(VERSION 3.7.1)

project(Game)

include(CTest)
include(src/common/Catch.cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

include(src/game/main.cmake)
include(src/TaskSystem/main.cmake)