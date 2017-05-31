project(FoundationTimer)
cmake_minimum_required(VERSION 2.8.12)

conan_basic_setup()

add_executable(timer timer.cpp)
target_link_libraries(timer ${CONAN_LIBS})
