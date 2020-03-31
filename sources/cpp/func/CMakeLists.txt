cmake_minimum_required(VERSION 3.7.1)

project(func)

include(CTest)
include(Catch.cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

list(APPEND APP_FILES main.cpp)
list(APPEND APP_FILES func.h)
list(APPEND APP_FILES events.h)

set (APP_NAME "func_tests")
add_executable(${APP_NAME} ${APP_FILES})
set_property(TARGET ${APP_NAME} PROPERTY CXX_STANDARD 20)
set_property(TARGET ${APP_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_target_properties(${APP_NAME} PROPERTIES LINK_FLAGS /SUBSYSTEM:CONSOLE)

## VULKAN
if (NOT "$ENV{VK_SDK_PATH}" STREQUAL "")
    target_include_directories(${APP_NAME} PRIVATE "$ENV{VK_SDK_PATH}/Include")  
    target_link_directories(${APP_NAME} PRIVATE "$ENV{VK_SDK_PATH}/Lib")  
    target_link_libraries(${APP_NAME} vulkan-1.lib)
    message("Vulkan, include @ [$ENV{VK_SDK_PATH}/Include], lib @ [$ENV{VK_SDK_PATH}/Lib]")
endif()

catch_discover_tests(${APP_NAME})