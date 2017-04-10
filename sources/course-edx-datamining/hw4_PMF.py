#!/usr/bin/python

import os
os.path.dirname(os.path.abspath(__file__))

import sys
import numpy

ratings = "ratings.csv"
if(len(sys.argv) >= 2):
    ratings = sys.argv[1]
 
print(ratings)

d = 5 
sigma2 = 1.0/10.0
lambdaa = 2.0 
iterations = 50

def printMatrixShape(name, M):
    print(name + ": " + str(numpy.shape(M)) + " (rows,columns)")

ratingsM = numpy.loadtxt(open(ratings, "rb"), delimiter=",", skiprows=0)

for row in range(0, numpy.shape(ratingsM)[0]):
    ratingsM[row,0] = ratingsM[row,0] - 1
    ratingsM[row,1] = ratingsM[row,1] - 1

printMatrixShape("ratingsM", ratingsM)

usersA = numpy.unique(ratingsM[:,0])
N1 = int(numpy.max(usersA)+1)
moviesA = numpy.unique(ratingsM[:,1])
N2 = int(numpy.max(moviesA)+1)

M = numpy.matrix(numpy.repeat(0.0, N1 * N2))
M = numpy.reshape(M, (N1, N2))

printMatrixShape("M", M)

def _omega():
    return [[i,j] for i in range(0,N1) for j in range(0,N2) if M[i,j] > 0.0]
preCalcOmega = _omega()
def omega():
    return preCalcOmega
    
def _omegaui(i):
    return [indices[1] for indices in omega() if indices[0] == i]
def _omegavj(j):
    return [indices[0] for indices in omega() if indices[1] == j]
preCalcOmegaUi = {i:_omegaui(i) for i in range(0,N1)}
preCalcOmegaVj = {j:_omegavj(j) for j in range(0,N2)}
def omegaui(i):
    return preCalcOmegaUi[i]
def omegavi(i):
    return preCalcOmegaVj[i]

for rating in ratingsM:
    M[int(rating[0]),int(rating[1])] = rating[2]

uM = numpy.matrix(numpy.repeat(0.0, N1 * d))
uM = numpy.reshape(uM, (N1, d))

vM = numpy.matrix(numpy.repeat(0.0, d * N2))
vM = numpy.reshape(vM, (d, N2))

printMatrixShape("uM", uM)
printMatrixShape("vM", vM)

def ui(i):
    return numpy.reshape(numpy.matrix(uM[i,:]), (1,d))
def setUi(i, r):
    for ix in range(0,numpy.size(r)):
        uM[i,ix] = r[ix]
def vj(j):
    return numpy.reshape(numpy.matrix(vM[:,int(j)]), (d,1))
def setVj(i, r):
    for ix in range(0,numpy.size(r)):
        vM[ix,i] = r[ix]
def Mij(i,j):
    return M[int(i),int(j)]

for i in range(0,N1):
    setUi(i, numpy.random.normal(0.0,1.0, d))
for j in range(0,N2):
    setVj(j, numpy.random.normal(0.0,1.0, d))

def I(n):
    return numpy.matrix(numpy.identity(n))
def T(v):
    return numpy.transpose(v)
 
def printMatrix(path, matrix2print):
    f = open(path, 'w')
    shape = numpy.shape(matrix2print)
    for row in range(0, shape[0]):
        for col in range(0, shape[1]):
            f.write('%.5f' % matrix2print[row,col])
            if(col != shape[1]-1):
                f.write(",")
        if(row != shape[0]-1):
            f.write("\n")
    f.close()
    
def summ(l,r,c):
    if len(l) == 0:
        return numpy.zeros((r,c))
    return numpy.sum(l, axis=0)
    
from datetime import datetime

fobj = open("objective.csv", 'w')
for iteration in range(0,iterations):
    print("iteration:"+ str(iteration) + ":" + str(datetime.now().time()))
    for i in range(0,N1):
        #u_i = \big[\lambda\sigma^2I + \sum_{j\in \omega ui}{v_jv^T}\big]*\big[\sum_{j\in \omega u_i}{M_{ij}v_j}\big]
        #sumofvjvjt = numpy.sum([numpy.dot(vj(j),T(vj(j))) for j in omegaui(i)], 0)
        sumofvjvjt = numpy.sum([numpy.dot(vj(j),T(vj(j))) for j in omegaui(i)], 0)
        item1 = (lambdaa*sigma2)*I(d)
        itemiI = numpy.linalg.inv(item1 + sumofvjvjt)
        item2 = summ([Mij(i,j)*vj(j) for j in omegaui(i)],d,1)
        result = numpy.dot(itemiI, item2) 
        setUi(i, result)
    for j in range(0,N2):
        sumofujujt = numpy.sum([numpy.dot(T(ui(i)),ui(i))
            for i in omegavi(j)], 0)
        item1 = (lambdaa*sigma2)*I(d)
        itemiI = numpy.linalg.inv(item1 + sumofujujt)
        item2 = summ([Mij(i,j)*ui(i) for i in omegavi(j)],1,d)
        result = numpy.dot(itemiI, T(item2))
        setVj(i, result)
    def objfirst(i,j):
        dot = (M[i,j] - numpy.dot(ui(i),vj(j))) 
        dot = dot*dot
        dot = (1.0/(2.0*sigma2))*dot
        return dot
    def objsecond(i):
        l = numpy.linalg.norm(ui(i)) * numpy.linalg.norm(ui(i))
        return (lambdaa/2.0)*l
    def objthird(i):
        l = numpy.linalg.norm(vj(i)) * numpy.linalg.norm(vj(i))
        return (lambdaa/2.0)*l
    objfirstpart = numpy.sum([objfirst(indices[0],indices[1])
        for indices in omega()])
    objsecondpart = numpy.sum([objsecond(i)
        for i in range(0,N1)])
    objthirdpart = numpy.sum([objthird(j)
        for j in range(0,N2)])
    obj = -(objfirstpart + objsecondpart + objthirdpart)
    fobj.write('%.5f' % obj)
    fobj.write("\n") 
    if iteration == 9 or iteration == 24 or iteration == 49:
        printMatrix("U-" + str(iteration+1) + ".csv", uM)
        printMatrix("V-" + str(iteration+1) + ".csv", T(vM))

fobj.close()
printMatrixShape("M",numpy.dot(uM,vM))
print("ok")