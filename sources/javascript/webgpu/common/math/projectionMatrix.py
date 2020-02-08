#!/usr/bin/python3
# 8.3 - Perspective Projections
# http://learnwebgl.brown37.net/08_projections/projections_perspective.html
from sympy import *
left,right,bottom,top,near,far,zmin,zmax = symbols('left right bottom top near far zmin zmax')

Z = Matrix([
    [1/near,1,0],
    [1/far,1,1]
]).rref()

zmin = simplify(Z[0][0,2])
zmax = simplify(Z[0][1,2])

A = Matrix([
    [2/(right-left),0,0,0],
    [0,2/(top-bottom),0,0],
    [0,0,1,0],
    [0,0,0,1]
])
B = Matrix([
    [near,0,0,0],
    [0,near,0,0],
    [0,0,1,0],
    [0,0,0,1]
])
C = Matrix([
    [1,0,0,0],
    [0,1,0,0],
    [0,0,-zmax,zmin],
    [0,0,-1,0]
])
D = Matrix([
    [1,0,0,-(left+right)/2],
    [0,1,0,-(bottom+top)/2],
    [0,0,1,0],
    [0,0,0,1]
])

R = A*B*C*D

[
    print("R[{0}] = {1};".format(
        j * 4 + i,
        ccode(simplify(R[i,j])))
    )
    for j in range(4)
    for i in range(4)    
]