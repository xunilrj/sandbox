cmake_minimum_required(VERSION 2.8)
project ("bitfield")

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()


add_executable(bitfield main.cxx)
target_link_libraries(bitfield ${CONAN_LIBS})
