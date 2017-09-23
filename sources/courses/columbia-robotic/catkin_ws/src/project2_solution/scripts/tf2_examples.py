#!/usr/bin/env python  
import rospy

import numpy

import tf
import tf2_ros
import geometry_msgs.msg

def publish_transforms():
    t1 = geometry_msgs.msg.TransformStamped()
    t1.header.stamp = rospy.Time.now()
    t1.header.frame_id = "world"
    t1.child_frame_id = "F1"
    t1.transform.translation.x = 1.0
    t1.transform.translation.y = 1.0
    t1.transform.translation.z = 0.0
    q1 = tf.transformations.quaternion_from_euler(1.0, 1.0, 1.0)
    t1.transform.rotation.x = q1[0]
    t1.transform.rotation.y = q1[1]
    t1.transform.rotation.z = q1[2]
    t1.transform.rotation.w = q1[3]
    br.sendTransform(t1)

    t2 = geometry_msgs.msg.TransformStamped()
    t2.header.stamp = rospy.Time.now()
    t2.header.frame_id = "F1"
    t2.child_frame_id = "F2"
    t2.transform.translation.x = 1.0
    t2.transform.translation.y = 0.0
    t2.transform.translation.z = 0.0
    q2 = tf.transformations.quaternion_about_axis(1.57, (1,0,0))
    t2.transform.rotation.x = q2[0]
    t2.transform.rotation.y = q2[1]
    t2.transform.rotation.z = q2[2]
    t2.transform.rotation.w = q2[3]
    br.sendTransform(t2)

    T1 = numpy.dot(tf.transformations.translation_matrix((1.0, 1.0, 0.0)),
                   tf.transformations.quaternion_matrix(q1))
    T1_inverse = tf.transformations.inverse_matrix(T1)
 
    t3 = geometry_msgs.msg.TransformStamped()
    t3.header.stamp = rospy.Time.now()
    t3.header.frame_id = "F3"
    t3.child_frame_id = "F4"
    tr3 = tf.transformations.translation_from_matrix(T1_inverse)
    t3.transform.translation.x = tr3[0]
    t3.transform.translation.y = tr3[1]
    t3.transform.translation.z = tr3[2]
    q3 = tf.transformations.quaternion_from_matrix(T1_inverse)
    t3.transform.rotation.x = q3[0]
    t3.transform.rotation.y = q3[1]
    t3.transform.rotation.z = q3[2]
    t3.transform.rotation.w = q3[3]
    br.sendTransform(t3)

    T2 = numpy.dot(tf.transformations.translation_matrix((1.0, 0.0, 0.0)),
                   tf.transformations.quaternion_matrix(q2))
    T2_inverse = tf.transformations.inverse_matrix(T2)

    t4 = geometry_msgs.msg.TransformStamped()
    t4.header.stamp = rospy.Time.now()
    t4.header.frame_id = "F2"
    t4.child_frame_id = "F3"
    tr4 = tf.transformations.translation_from_matrix(T2_inverse)
    t4.transform.translation.x = tr4[0]
    t4.transform.translation.y = tr4[1]
    t4.transform.translation.z = tr4[2]
    q4 = tf.transformations.quaternion_from_matrix(T2_inverse)
    t4.transform.rotation.x = q4[0]
    t4.transform.rotation.y = q4[1]
    t4.transform.rotation.z = q4[2]
    t4.transform.rotation.w = q4[3]
    br.sendTransform(t4)

if __name__ == '__main__':
    rospy.init_node('tf2_examples')

    br = tf2_ros.TransformBroadcaster()
    rospy.sleep(0.5)

    while not rospy.is_shutdown():
        publish_transforms()
        rospy.sleep(0.05)
