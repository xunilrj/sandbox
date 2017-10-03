#!/bin/bash
cd ./catkin_ws
source ./devel/setup.bash
roscore &
sleep 10
rosparam set robot_description --textfile /home/vagrant/proj03/kuka_lwr_arm.urdf
sleep 5
rosrun robot_sim robot_sim_bringup &
sleep 5
rosrun robot_mover mover &