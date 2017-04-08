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
sigma2 = 1/10
lambdaa = 2 
iterations = 50

ratingsM = numpy.loadtxt(open(ratings, "rb"), delimiter=",", skiprows=0)

usersA = numpy.unique(ratingsM[:,0])
usersQtd = int(numpy.max(usersA) + 1)
moviesA = numpy.unique(ratingsM[:,1])
moviesQtd = int(numpy.max(moviesA) + 1)

M = numpy.matrix(numpy.repeat(0.0, usersQtd * moviesQtd))
M = numpy.reshape(M, (usersQtd, moviesQtd))

for rating in ratingsM:
    M[int(rating[0]),int(rating[1])] = rating[2]

uM = numpy.matrix(numpy.repeat(0, d * moviesQtd))
uM = numpy.reshape(uM, (d, moviesQtd))

vM = numpy.matrix(numpy.repeat(0, d * moviesQtd))
vM = numpy.reshape(vM, (d, moviesQtd))

def I(n):
    return numpy.matrix(numpy.identity(n))
def T(v):
    return numpy.transpose(v)

iteration = 0
for iteration in range(0,iterations):
    for i in range(0,usersQtd):
        #u_i = \big[\lambda\sigma^2I + \sum_{j\in \omega ui}{v_jv^T}\big]*\big[\sum_{j\in \omega u_i}{M_{ij}v_j}\big]
        indices = numpy.where(ratingsM[:,0] == i)
        uiRatings = ratingsM[indices]
        js = numpy.unique(uiRatings[:,1])
        sumofvjvjt = numpy.sum([numpy.dot(vM[:,int(j)],T(vM[:,int(j)])) for j in js], 0)
        item1 = (lambdaa*sigma2)*I(d)
        itemiI = numpy.linalg.inv(item1 + sumofvjvjt)
        item2 = numpy.sum([M[int(i),int(j)]*vM[:,int(j)] for j in js], 0)
        result = numpy.dot(itemiI, item2) 
        for ix in range(0, numpy.size(result)):
            uM[i,ix] = result[ix]
        
#for j in range(0,moviesQtd):
        #print(j)

#print(usersM)
#print(moviesM)