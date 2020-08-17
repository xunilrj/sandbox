import sympy

u, v1, v2, l = sympy.symbols('u, v1, v2, l')
def J(U,V,l):
    return 1/2*(1-U[0]*V[1])**2+l*(U[0]*U[0] + V[0]*V[0])

U0 = sympy.Matrix([u,1])
V0 = sympy.Matrix([v1,v2])
J0 = J(U0,V0,l)
print("Before Step 0; Y = ", U0*V0.T)
print("U = ", U0)
print("V = ", V0)
print("J = ", J0)

J0dv1 = sympy.Derivative(J0, v1).doit()
solv1 = sympy.solveset(sympy.Eq(J0dv1, 0), v1)
J0dv2 = sympy.Derivative(J0, v2).doit()
solv2 = sympy.solveset(sympy.Eq(J0dv2, 0), v2)
V0 = sympy.Matrix([solv1.args[0],solv2.args[0]])

print("J0dv1 = ", J0dv1)
print("J0dv2 = ", J0dv2)
print("End Step 0");

U1 = U0
V1 = V0
J1 = J(U1,V1,l)
print("Before Step 1; Y = ", U1*V1.T)
print("U = ", U1)
print("V = ", V1)
print("J = ", J1)

J1du = sympy.Derivative(J1, u).doit()
solu = sympy.solveset(sympy.Eq(J1du, 0), u)

print("J1du = ", J1du)
print("solu = ", solu)

U1 = U1.replace(u, 0)
V1 = V1.replace(u, 0)

print("U = ", U1)
print("V = ", V1)

from numpy import matrix
from numpy.linalg import * 

def rank(M):
    m = matrix(M)
    print(m)
    print(matrix_rank(m))

rank([[1,-1],[-1,1]])
rank([[1,0],[0,1]])
rank([[1,1],[-1,-1]])

