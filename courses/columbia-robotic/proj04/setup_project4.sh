source /opt/ros/indigo/setup.bash

export ROS_NODE_PORT=`get_free_port.py`
export ROS_MASTER_URI=http://localhost:$ROS_NODE_PORT

lf="ros.log"
if [ -e $lf ]; then
  \rm $lf
fi
touch $lf

echo "Starting roscore with port = $ROS_NODE_PORT..."
( ( (stdbuf -oL roscore -p $ROS_NODE_PORT) 1> >(stdbuf -oL sed 's/^/ROSCORE: /') 2>&1 ) >> $lf ) &

sleep 1

echo "Running web publisher..."
( ( (stdbuf -oL rosrun tf2_web_republisher tf2_web_republisher) 1> >(stdbuf -oL sed 's/^/PUB: /') 2>&1 ) >> $lf ) &

sleep 1

echo "Launching RosBridge server..."
( ( (stdbuf -oL ./launch_rosbridge_server.py) 1> >(stdbuf -oL sed 's/^/BR: /') 2>&1 ) >> $lf ) &

sleep 1

echo "Launching interactive_marker_proxy..."
( ( (stdbuf -oL rosrun interactive_marker_proxy proxy topic_ns:=/control_markers target_frame:=/world_link) 1> >(stdbuf -oL sed 's/^/PROXY: /') 2>&1 ) >> $lf ) &


cd catkin_ws
catkin_make
source devel/setup.bash
cd ..

rosparam set robot_description --textfile kuka_lwr_arm.urdf

echo "Launching robot_sim..."
( ( (stdbuf -oL rosrun robot_sim robot_sim_bringup) 1> >(stdbuf -oL sed 's/^/KUKA: /') 2>&1 ) >> $lf ) &

sleep 1

echo "Launching robot_state_publisher..."
( ( (stdbuf -oL rosrun robot_state_publisher robot_state_publisher) 1> >(stdbuf -oL sed 's/^/STATE_PUB: /') 2>&1 ) >> $lf ) &

sleep 1

echo "Launching marker_control..."
rosrun cartesian_control marker_control.py &