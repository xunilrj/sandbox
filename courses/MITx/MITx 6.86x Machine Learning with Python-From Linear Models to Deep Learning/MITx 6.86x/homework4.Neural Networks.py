import numpy as np

def ReLU(v):
    return max(v,0)
f = np.vectorize(ReLU)

x = np.array([3,14,1])
W = np.matrix('1,0,-1;0,1,-1;-1,0,-1;0,-1,-1')
V = np.matrix('1,1,1,1,0;-1,-1,-1,-1,2')

# Feed Forward Step

z = f(W@x)
ez = np.column_stack([z,1])

u = f(ez@V.T)

from scipy.special import softmax
o = softmax(u)

np.set_printoptions(suppress=True,formatter={'float': lambda x: "{0:0.10f}".format(x)})
print(o)

# Decision Boundaries
import sympy

W = sympy.Matrix(([1,0,-1],[0,1,-1],[-1,0,-1],[0,-1,-1]))
V = sympy.Matrix(([1,1,1,1,0],[-1,-1,-1,-1,2]))

x1,x2 = sympy.symbols('x1, x2')

x = sympy.Matrix([x1,x2,1])
print(W*x)

# Output of Neural Network

fz1, fz2, fz3, fz4 = sympy.symbols('fz1, fz2, fz3, fz4')

# exp(fz1 + fz2 + fz3 + fz4), exp(-fz1 - fz2 - fz3 - fz4 + 2)

u=V*sympy.Matrix([fz1,fz2,fz3,fz4,1])
print(sympy.exp(u[0])+sympy.exp(u[1]))

print("f(z1)+f(z2)+f(z3)+f(z4)=1")
print(
    sympy.exp(1) / (sympy.exp(1)+sympy.exp(1))
)

print("f(z1)+f(z2)+f(z3)+f(z4)=0")
print(
    sympy.exp(0) / (sympy.exp(0)+sympy.exp(2))
)

print("f(z1)+f(z2)+f(z3)+f(z4)=3")
print(
    sympy.exp(3) / (sympy.exp(3)+sympy.exp(0))
)

# Inverse Temperature


