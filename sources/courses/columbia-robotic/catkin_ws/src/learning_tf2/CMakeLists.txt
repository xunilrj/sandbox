cmake_minimum_required(VERSION 2.8.3)
project(learning_tf2)

find_package(catkin REQUIRED COMPONENTS
  roscpp
  rospy
  tf2
  tf2_ros
  turtlesim
)

catkin_package()

include_directories(${catkin_INCLUDE_DIRS})

add_executable(turtle_tf2_broadcaster src/turtle_tf2_broadcaster.cpp)
target_link_libraries(turtle_tf2_broadcaster ${catkin_LIBRARIES})

add_executable(turtle_tf2_listener src/turtle_tf2_listener.cpp)
target_link_libraries(turtle_tf2_listener ${catkin_LIBRARIES})

install(FILES start_demo.launch DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
