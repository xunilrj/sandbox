#!/usr/bin/python3
# 8.3 - Perspective Projections
# http://learnwebgl.brown37.net/08_projections/projections_perspective.html
from math import *
from sympy import *
left,right,bottom,top,near,far = symbols('left right bottom top near far')

# near-far range
# OpenGL -1 to 1
# D3D, webGPU 0 to 1
# Solve system
# c1/z + c2
# When z = near, the equation must calculate 0.
# When z = far, the equation must calculate 1.
Z = Matrix([
    [1/near, 1,  0],
    [1/far,  1,  1]
]).rref()

z1 = simplify(Z[0][0,2])
z2 = simplify(Z[0][1,2])
#print(z1)
#print(z2)

# Scale the 2D (x’,y’) values 
# in the viewing window to a 
# 2-by-2 unit square; (-1,-1) to (+1,+1)
A = Matrix([
    [2/(right-left),             0, 0, 0],
    [             0,2/(top-bottom), 0, 0],
    [             0,             0, 1, 0],
    [             0,             0, 0, 1]
])
# Perform the perspective calculation
B = Matrix([
    [near,   0, 0, 0],
    [   0,near, 0, 0],
    [   0,   0, 1, 0],
    [   0,   0, 0, 1]
])
# Scale the depth values (z) 
# into a normalized range (0,+1) 
# (and setup for division by (z)). 
# c1, c2, x, y, z = symbols('c1 c2 x y z')
# C = Matrix([
#     [1, 0,    0,    0],
#     [0, 1,    0,    0],
#     [0, 0,   c2,   c1],
#     [0, 0,    1,    0]
# ])
# print(C*Matrix([[x],[y],[z],[1]]))
C = Matrix([
    [1, 0,    0,    0],
    [0, 1,    0,    0],
    [0, 0,   z2,   z1],
    [0, 0,    1,    0]
])

# Translate the apex of the frustum to the origin.
D = Matrix([
    [1, 0, 0, -(left+right)/2],
    [0, 1, 0, -(bottom+top)/2],
    [0, 0, 1,               0],
    [0, 0, 0,               1]
])

R = A*B*C*D

P = R.evalf(subs={left:-1,right:1,bottom:-1,top:1,near:2,far:40})
M1 = P*Matrix([[ 1],[ 1],[ 2],[1]]); M1 = M1/M1[3]; #print(M1)
M2 = P*Matrix([[-1],[-1],[ 2],[1]]); M2 = M2/M2[3]; #print(M2)
M3 = P*Matrix([[ 1],[ 1],[40],[1]]); M3 = M3/M3[3]; #print(M3)
M4 = P*Matrix([[-1],[-1],[40],[1]]); M4 = M4/M4[3]; #print(M4)

assert (isclose(M1[0], 1)), "Must be at far right"
assert (isclose(M1[1], 1)), "Must be at up"
assert (isclose(M1[2], 0)), "Must be at zero"
assert (isclose(M2[0], -1)), "Must be at far left"
assert (isclose(M2[1], -1)), "Must be at bottom"
assert (isclose(M2[2],  0)), "Must be at ozerone"
# Perfect center in infinite and not the "far" point
assert (isclose(M3[0], 0.05)), "Must be near center, but +x+y quadrant"
assert (isclose(M3[1], 0.05)), "Must be near center, but +x+y quadrant"
assert (isclose(M3[2], 1)), "Must be at one"
assert (isclose(M4[0],-0.05)), "Must be near center, but -x-y quadrant"
assert (isclose(M4[1],-0.05)), "Must be near center, but -x-y quadrant"
assert (isclose(M4[2], 1)), "Must be at one"

[
    print("R[{0}] = {1};".format(
        j * 4 + i,
        ccode(simplify(R[i,j])))
    )
    for j in range(4)
    for i in range(4)    
]