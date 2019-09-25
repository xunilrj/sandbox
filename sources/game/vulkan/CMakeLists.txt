cmake_minimum_required(VERSION 3.7.1)
set (Recipe_Name "3_0_DeviceHandshake")

set(VULKAN_SDK "C:/VulkanSDK")
set(VULKAN_VERSION "1.1.114.0")
set(VULKAN_PATH "${VULKAN_SDK}/${VULKAN_VERSION}")
set(Vulkan_LIBRARIES "vulkan-1")

project(${Recipe_Name})
add_definitions(-DVK_USE_PLATFORM_WIN32_KHR)

set(VULKAN_LINK_LIST "vulkan-1")
if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
	include_directories(AFTER ${VULKAN_PATH}/Include)
	link_directories(${VULKAN_PATH}/Lib)
endif()

if (WIN32)
	source_group ("include" REGULAR_EXPRESSION "include/*")
	source_group ("src" REGULAR_EXPRESSION "src/*")
endif (WIN32)

include_directories (${CMAKE_CURRENT_SOURCE_DIR}/include)
file (GLOB_RECURSE CPP_FILES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp)
file (GLOB_RECURSE HPP_FILES ${CMAKE_CURRENT_SOURCE_DIR}/include/*.*)
add_executable (${Recipe_Name} ${CPP_FILES} ${HPP_FILES})
target_link_libraries (${Recipe_Name} ${Vulkan_LIBRARIES})
message(STATUS "VULKAN LIBS ${Vulkan_LIBRARIES}")

set_property(TARGET ${Recipe_Name} PROPERTY CXX_STANDARD 17)
set_property(TARGET ${Recipe_Name} PROPERTY CXX_STANDARD_REQUIRED ON)
set_target_properties(${Recipe_Name} PROPERTIES LINK_FLAGS /SUBSYSTEM:WINDOWS)