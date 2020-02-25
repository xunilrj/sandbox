cmake_minimum_required(VERSION 3.7.1)

project(vulkan)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

list(APPEND APP_FILES main.cpp)
list(APPEND APP_FILES models/Boy01_Head_GeoMesh.vulkan.h)
list(APPEND APP_FILES shaders/vert.h)
list(APPEND APP_FILES vulkan.h)

set (APP_NAME "vulkan")
add_executable(${APP_NAME} ${APP_FILES})
set_property(TARGET ${APP_NAME} PROPERTY CXX_STANDARD 20)
set_property(TARGET ${APP_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
target_compile_options(${APP_NAME} PRIVATE "/await")
set_target_properties(${APP_NAME} PROPERTIES LINK_FLAGS /SUBSYSTEM:WINDOWS)
set_target_properties(${APP_NAME} PROPERTIES 
    VS_DEBUGGER_WORKING_DIRECTORY "$(SolutionDir)..")

## VULKAN
if (NOT "$ENV{VK_SDK_PATH}" STREQUAL "")
    target_include_directories(${APP_NAME} PRIVATE "$ENV{VK_SDK_PATH}/Include")  
    target_link_directories(${APP_NAME} PRIVATE "$ENV{VK_SDK_PATH}/Lib")  
    target_link_libraries(${APP_NAME} vulkan-1.lib)
    message("Vulkan: include @ [$ENV{VK_SDK_PATH}/Include], lib @ [$ENV{VK_SDK_PATH}/Lib]")
endif()