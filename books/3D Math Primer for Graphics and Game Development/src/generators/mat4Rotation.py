#!/usr/bin/python3
# 8.3 - Perspective Projections
# http://learnwebgl.brown37.net/08_projections/projections_perspective.html
from math import *
from sympy import *
vx, vy, vz = symbols('vx vy vz')
nx, ny, nz = symbols('nx ny nz')
theta = symbols('theta')


def vdot(a,b):
    return simplify(a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
def vmulf(f,a):
    return [
        simplify(f*a[0]),
        simplify(f*a[1]),
        simplify(f*a[2])
    ]
def vproj(v,n):
    return vmulf(vdot(v,n), n)
def vadd(a,b):
    return [
        simplify(a[0]+b[0]),
        simplify(a[1]+b[1]),
        simplify(a[2]+b[2]) 
    ]
def vdiff(a,b):
    return [
        simplify(a[0]-b[0]),
        simplify(a[1]-b[1]),
        simplify(a[2]-b[2]) 
    ]
def vcross(a,b):
    return [
        simplify(a[1]*b[2]-a[2]*b[1]),
        simplify(a[2]*b[0]-a[0]*b[2]),
        simplify(a[0]*b[1]-a[1]*b[0]) 
    ]
def vrot(vx,vy,vz):
    v = [vx,vy,vz]
    n = [nx,ny,nz]
    vpar = vproj(v,n)
    vper = vdiff(v,vpar)
    w = vcross(n, vper)
    vprimeper = vadd(vmulf(cos(theta), vper), vmulf(sin(theta), w))
    vprime = vadd(vpar, vprimeper)
    return vprime

p = vrot(1,0,0)
q = vrot(0,1,0)
r = vrot(0,0,1)

R = Matrix([
    [p[0], p[1], p[2], 0],
    [q[0], q[1], q[2], 0],
    [r[0], r[1], r[2], 0],
    [0, 0, 0, 1]
])

[
    print("R[{0}] = {1};".format(
        j * 4 + i,
        ccode(simplify(R[i,j])))
    )
    for j in range(4)
    for i in range(4)    
]