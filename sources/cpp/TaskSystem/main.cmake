list(APPEND TASKSYSTEM_FILES src/TaskSystem/TaskSystem.h)
list(APPEND TASKSYSTEM_FILES src/TaskSystem/tests.cpp)

set (appName "tests_task_system")
add_executable(${appName} ${TASKSYSTEM_FILES})
set_property(TARGET ${appName} PROPERTY CXX_STANDARD 17)
set_property(TARGET ${appName} PROPERTY CXX_STANDARD_REQUIRED ON)
set_target_properties(${appName} PROPERTIES LINK_FLAGS /SUBSYSTEM:CONSOLE)

include(CTest)

include(src/common/Catch.cmake)
catch_discover_tests(${appName})
