#!/usr/bin/env python

import numpy
import rospy
import tf
import tf2_ros
import geometry_msgs.msg
import math

def normalize(v):
    norm=numpy.linalg.norm(v)
    if norm==0: 
       return v
    return v/norm

def lookupMatrixLH(cameraPosition, cameraTarget, cameraUpVector):
    zaxis = normalize(cameraTarget - cameraPosition)
    xaxis = normalize(numpy.cross(cameraUpVector, zaxis))
    yaxis = numpy.cross(zaxis, xaxis)
    array = numpy.array([
        xaxis[0], yaxis[0], zaxis[0], 0,
        xaxis[1], yaxis[1], zaxis[1], 0,
        xaxis[2], yaxis[2], zaxis[2], 0,
        -numpy.dot(xaxis, cameraPosition),
        -numpy.dot(yaxis, cameraPosition),
        -numpy.dot(zaxis, cameraPosition),
        1], dtype=float)
    return array.reshape(4, 4, order='F')
def lookupMatrixRH(cameraPosition, cameraTarget, cameraUpVector):
    zaxis = normalize(cameraPosition - cameraTarget)
    xaxis = normalize(numpy.cross(cameraUpVector, zaxis))
    yaxis = numpy.cross(zaxis, xaxis)
    array = numpy.array([
        xaxis[0], yaxis[0], zaxis[0], 0,
        xaxis[1], yaxis[1], zaxis[1], 0,
        xaxis[2], yaxis[2], zaxis[2], 0,
        -numpy.dot(xaxis, cameraPosition),
        -numpy.dot(yaxis, cameraPosition),
        -numpy.dot(zaxis, cameraPosition),
        1], dtype=float)
    return array.reshape(4, 4, order='F')

def publish_transforms():
    TBase2Object = numpy.dot(
        tf.transformations.quaternion_matrix(
            tf.transformations.quaternion_from_euler(0.79, 0.0, 0.79)
        ),
        tf.transformations.translation_matrix((0.0, 1.0, 1.0))
    )
    TObject2Base = tf.transformations.inverse_matrix(TBase2Object)
    tr = tf.transformations.translation_from_matrix(TBase2Object)
    q = tf.transformations.quaternion_from_matrix(TBase2Object)
    object_transform = geometry_msgs.msg.TransformStamped()
    object_transform.transform.translation.x = tr[0]
    object_transform.transform.translation.y = tr[1]
    object_transform.transform.translation.z = tr[2]   
    object_transform.transform.rotation.x = q[0]
    object_transform.transform.rotation.y = q[1]
    object_transform.transform.rotation.z = q[2]
    object_transform.transform.rotation.w = q[3]
    object_transform.header.stamp = rospy.Time.now()
    object_transform.header.frame_id = "base_frame"
    object_transform.child_frame_id = "object_frame"
    br.sendTransform(object_transform)

    TBase2Robot = numpy.dot(
        tf.transformations.quaternion_matrix(
            tf.transformations.quaternion_from_euler(0.0, 0.0, 1.5)
        ),
        tf.transformations.translation_matrix((0.0, -1.0, 0.0))
    )
    tr = tf.transformations.translation_from_matrix(TBase2Robot)
    q = tf.transformations.quaternion_from_matrix(TBase2Robot)
    robot_transform = geometry_msgs.msg.TransformStamped()
    robot_transform.transform.translation.x = tr[0]
    robot_transform.transform.translation.y = tr[1]
    robot_transform.transform.translation.z = tr[2]   
    robot_transform.transform.rotation.x = q[0]
    robot_transform.transform.rotation.y = q[1]
    robot_transform.transform.rotation.z = q[2]
    robot_transform.transform.rotation.w = q[3]
    robot_transform.header.stamp = rospy.Time.now()
    robot_transform.header.frame_id = "base_frame"
    robot_transform.child_frame_id = "robot_frame"
    br.sendTransform(robot_transform)

    TRobot2Object = numpy.dot(TObject2Base, TBase2Robot)
    TObject2Robot = tf.transformations.inverse_matrix(TRobot2Object)

    objp = numpy.dot(TObject2Robot, numpy.array([0,0,0,1]))
    camp = numpy.array([0,0.1,0.1,1])

    TRobot2Camera = tf.transformations.translation_matrix(camp)
    TCamera2Base = numpy.dot(TBase2Robot,TRobot2Camera)
    TBase2Camera = tf.transformations.inverse_matrix(TCamera2Base)    
    
    camv = numpy.array([1,0,0,1])[:3]
    targetv = normalize(numpy.subtract(objp, camp))[:3]
    axis = normalize(numpy.cross(camv, targetv))
    angle = math.acos(numpy.dot(camv, targetv))
        
    tr = tf.transformations.translation_from_matrix(TRobot2Camera)
    q = tf.transformations.quaternion_about_axis(angle, axis)
    
    camera_transform = geometry_msgs.msg.TransformStamped()
    camera_transform.transform.translation.x = tr[0]
    camera_transform.transform.translation.y = tr[1]
    camera_transform.transform.translation.z = tr[2]
    camera_transform.transform.rotation.x = q[0]
    camera_transform.transform.rotation.y = q[1]
    camera_transform.transform.rotation.z = q[2]
    camera_transform.transform.rotation.w = q[3] 
    camera_transform.header.stamp = rospy.Time.now()
    camera_transform.header.frame_id = "robot_frame"
    camera_transform.child_frame_id = "camera_frame"
    br.sendTransform(camera_transform)

if __name__ == '__main__':
    rospy.init_node('project2_solution')

    br = tf2_ros.TransformBroadcaster()
    rospy.sleep(0.5)

    while not rospy.is_shutdown():
        publish_transforms()
        rospy.sleep(0.05)
