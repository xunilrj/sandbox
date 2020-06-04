cmake_minimum_required(VERSION 3.0.0)
project(histogram VERSION 0.1.0)

find_package(OpenCL REQUIRED)


add_executable(histogram main.cpp)
target_link_libraries(histogram PRIVATE OpenCL::OpenCL)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
