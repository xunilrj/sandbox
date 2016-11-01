cmake_minimum_required (VERSION 2.6)
project (eulerproblems)

set(eulerproblems_VERSION_MAJOR 0)
set(eulerproblems_VERSION_MINOR 1)

include_directories("./dependencies/include")

set (PROJECT_LINK_LIBS "../../dependencies/libs/Debug/x64")

file(GLOB SOURCES "sources/*.cpp")
add_executable(eulerproblems ${SOURCES})
target_link_libraries(eulerproblems "${PROJECT_LINK_LIBS}/gtest/gtest")

if(MSVC)
# We statically link to reduce dependencies
foreach(flag_var CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
    if(${flag_var} MATCHES "/MD")
        string(REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
    endif(${flag_var} MATCHES "/MD")
    if(${flag_var} MATCHES "/MDd")
        string(REGEX REPLACE "/MDd" "/MTd" ${flag_var} "${${flag_var}}")
    endif(${flag_var} MATCHES "/MDd")
endforeach(flag_var)
endif(MSVC)