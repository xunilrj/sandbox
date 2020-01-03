list(APPEND GAME_FILES src/game/main.cpp)
list(APPEND GAME_FILES src/TaskSystem/TaskSystem.h)

set (appName "game")
add_executable(${appName} ${GAME_FILES})
set_property(TARGET ${appName} PROPERTY CXX_STANDARD 17)
set_property(TARGET ${appName} PROPERTY CXX_STANDARD_REQUIRED ON)
set_target_properties(${appName} PROPERTIES LINK_FLAGS /SUBSYSTEM:CONSOLE)