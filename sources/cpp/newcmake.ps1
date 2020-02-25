param([Parameter(Mandatory=$true)]$Name)
[System.Environment]::CurrentDirectory = (gl).Path;
[System.IO.File]::WriteAllText("./CMakeLists.txt",
@"
cmake_minimum_required(VERSION 3.7.1)

project($Name)

include(CTest)
include(Catch.cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

list(APPEND APP_FILES main.cpp)

set (APP_NAME "$($Name)_tests")
add_executable(`${APP_NAME} `${APP_FILES})
set_property(TARGET `${APP_NAME} PROPERTY CXX_STANDARD 20)
set_property(TARGET `${APP_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_target_properties(`${APP_NAME} PROPERTIES LINK_FLAGS /SUBSYSTEM:CONSOLE)

## VULKAN
if (NOT "`$ENV{VK_SDK_PATH}" STREQUAL "")
    target_include_directories(`${APP_NAME} PRIVATE "`$ENV{VK_SDK_PATH}/Include")  
    target_link_directories(`${APP_NAME} PRIVATE "`$ENV{VK_SDK_PATH}/Lib")  
    target_link_libraries(`${APP_NAME} vulkan-1.lib)
    message("Vulkan, include @ [`$ENV{VK_SDK_PATH}/Include], lib @ [`$ENV{VK_SDK_PATH}/Lib]")
endif()

catch_discover_tests(`${APP_NAME})
"@)

if([System.IO.File]::Exists("main.cpp") -eq $false) {
    [System.IO.File]::WriteAllText("./main.cpp",
@"
    #define CATCH_CONFIG_MAIN 
    #include "../catch/catch.hpp"

    TEST_CASE("Fake.Test.Will Pass", "[ok]")
    {
    }
"@)
}

wget https://github.com/catchorg/Catch2/releases/download/v2.11.1/catch.hpp -OutFile catch.hpp
wget https://raw.githubusercontent.com/catchorg/Catch2/ac94bd05209d6dffb6aa7cb9750cfc45cbc4ac72/contrib/Catch.cmake -OutFile Catch.cmake
wget https://raw.githubusercontent.com/catchorg/Catch2/ac94bd05209d6dffb6aa7cb9750cfc45cbc4ac72/contrib/CatchAddTests.cmake -OutFile CatchAddTests.cmake
wget https://raw.githubusercontent.com/xunilrj/sandbox/master/sources/cpp/catch/ReferenceTests.runsettings -OutFile ReferenceTests.runsettings