cmake_minimum_required (VERSION 2.6)
project (cppserver)

set(cppserver_VERSION_MAJOR 0)
set(cppserver_VERSION_MINOR 1)

include_directories(dependencies)

set (PROJECT_LINK_LIBS "../../dependencies/bin/ace/static/ACEd")

file(GLOB SOURCES "sources/*.cpp")
add_executable(cppserver ${SOURCES})
target_link_libraries(cppserver ${PROJECT_LINK_LIBS})