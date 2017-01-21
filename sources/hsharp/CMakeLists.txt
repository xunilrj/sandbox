cmake_minimum_required (VERSION 2.6)
project (hsharp)

SET(Boost_INCLUDE_DIR "C:/github/boostorg-boost")
SET(Boost_LIBRARY_DIR "C:/github/boostorg-boost/bin.v2/libs")

FIND_PACKAGE(Boost)
IF (Boost_FOUND)
    INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIR})
ENDIF()

add_executable(hsharp source/main.cpp)
