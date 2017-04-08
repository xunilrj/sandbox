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

ratingsM = numpy.loadtxt(open(ratings, "rb"), delimiter=",", skiprows=0)

usersA = numpy.unique(ratingsM[:,0])
N1 = int(numpy.max(usersA) + 1)
moviesA = numpy.unique(ratingsM[:,1])
N2 = int(numpy.max(moviesA) + 1)

M = numpy.matrix(numpy.repeat(0.0, N1 * N2))
M = numpy.reshape(M, (N1, N2))

def omega():
    return [[i,j] for i in range(0,N1) for j in range(0,N2) if M[i,j] > 0.0]
def omegaui(i):
    return [indices[1] for indices in omega() if indices[0] == i]
def omegavi(i):
    return [indices[0] for indices in omega() if indices[1] == i]

for rating in ratingsM:
    M[int(rating[0]),int(rating[1])] = rating[2]

uM = numpy.matrix(numpy.repeat(0.0, d * N2))
uM = numpy.reshape(uM, (d, N2))

vM = numpy.matrix(numpy.repeat(0.0, d * N2))
vM = numpy.reshape(vM, (d, N2))

for i in range(0,N2):
    for ix in range(0, d):
            uM[i,ix] = numpy.random.normal(0.0,1.0)
            
for j in range(0,N2):
    for ix in range(0, d):
            vM[j,ix] = numpy.random.normal(0.0,1.0)
             
print(uM)
print(vM)

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
        f.write("\n")
    f.close()
    print(path)

fobj = open("objectives.csv", 'w')
iteration = 0
for iteration in range(0,iterations):    
    for i in range(0,N1):
        #u_i = \big[\lambda\sigma^2I + \sum_{j\in \omega ui}{v_jv^T}\big]*\big[\sum_{j\in \omega u_i}{M_{ij}v_j}\big]
        indices = numpy.where(ratingsM[:,0] == i)
        uiRatings = ratingsM[indices]
        js = numpy.unique(uiRatings[:,0])
        sumofvjvjt = numpy.sum([numpy.dot(vM[:,int(j)],T(vM[:,int(j)])) for j in js], 0)
        item1 = (lambdaa*sigma2)*I(d)
        itemiI = numpy.linalg.inv(item1 + sumofvjvjt)
        item2 = numpy.sum([M[int(i),int(j)]*vM[:,int(j)] for j in js], 0)
        result = numpy.dot(itemiI, item2) 
        for ix in range(0, numpy.size(result)):
            uM[i,ix] = result[ix]
    for j in range(0,N2):
        indices = numpy.where(ratingsM[:,1] == i)
        uiRatings = ratingsM[indices]
        iss = numpy.unique(uiRatings[:,1])
        sumofujujt = numpy.sum([numpy.dot(uM[:,int(i)],T(uM[:,int(i)])) for i in iss], 0)
        item1 = (lambdaa*sigma2)*I(d)
        itemiI = numpy.linalg.inv(item1 + sumofujujt)
        item2 = numpy.sum([M[int(i),int(j)]*uM[:,int(i)] for i in iss], 0)
        result = numpy.dot(itemiI, item2) 
        for ix in range(0, numpy.size(result)):
            vM[i,ix] = result[ix]
    def objfirst(i,j):
        dot = (M[i,j] - numpy.dot(T(uM[:,i]),vM[:,j]))
        dot = dot*dot
        dot = 1.0/(2.0*sigma2)
        return dot
    def objsecond(i):
        ui = uM[:,1]
        ui = numpy.linalg.norm(ui) * numpy.linalg.norm(ui)
        return (lambdaa/2.0)*ui
    def objthird(i):
        vi = vM[:,1]
        vi = numpy.linalg.norm(vi) * numpy.linalg.norm(vi)
        return (lambdaa/2.0)*vi
    objfirstpart = numpy.sum([objfirst(i,j) for i in range(0,N1) for j in range(0,N2)])
    objsecondpart = numpy.sum([objsecond(i) for i in range(0,N1)])
    objthirdpart = numpy.sum([objthird(j) for j in range(0,N2)])
    obj = objfirstpart + objsecondpart + objthirdpart
    fobj.write('%.5f' % obj)
    fobj.write("\n")
    if iteration == 9 or iteration == 24 or iteration == 49:
        printMatrix("U-" + str(iteration+1) + ".csv", uM)
        printMatrix("V-" + str(iteration+1) + ".csv", vM)

fobj.close() 
result = numpy.dot(T(vM),uM)
