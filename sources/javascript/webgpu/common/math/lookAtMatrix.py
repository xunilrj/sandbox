#!/usr/bin/python3
# 7.1 - Introduction to Cameras
# http://learnwebgl.brown37.net/07_cameras/camera_introduction.html
from sympy import *
eye, target, up = symbols('eye target up')

sub = Function('sub')
normalize = Function('normalize')
cross = Function('cross')


z = normalize(sub(eye, target))
x = normalize(cross(up, z))
y = normalize(cross(z, x))
print(x)
print(y)
print(z)
# const center = [center_x, center_y, center_z];
# const eye = [eye_x, eye_y, eye_z];
# const up = [up_dx, up_dy, up_dz];

# const z = vec3.normalize(vec3.sub(center, eye));
# const x = vec3.normalize(vec3.cross(up, z));
# const y = vec3.normalize(vec3.cross(z, x));

# var tx = -vec3.dot(x, eye);
# var ty = -vec3.dot(y, eye);
# var tz = vec3.dot(z, eye);

# mat4[0] = x[0];  mat4[4] = x[1]; mat4[8]  = x[2]; mat4[12] = tx;
# mat4[1] = y[0];  mat4[5] = y[1]; mat4[9]  = y[2]; mat4[13] = ty;
# mat4[2] = z[0];  mat4[6] = z[1]; mat4[10] = z[2]; mat4[14] = tz;
# mat4[3] = 0;     mat4[7] = 0;    mat4[11] = 0;    mat4[15] = 1;

# return mat4;