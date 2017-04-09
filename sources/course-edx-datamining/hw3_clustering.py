#!/usr/bin/python

import os
os.path.dirname(os.path.abspath(__file__))

import sys
import numpy

data = "X.csv"
if(len(sys.argv) >= 2):
    data = sys.argv[1]
 
print(data)

def printMatrixShape(name, M):
    print(name + ": " + str(numpy.shape(M)) + " (rows,columns)")
    
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
    
def summ(l,r,c):
    if len(l) == 0:
        return numpy.zeros((r,c))
    return numpy.sum(l, axis=0)

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab


dataM = numpy.loadtxt(open(data, "rb"), delimiter=",", skiprows=0)
printMatrixShape("dataM", dataM)

means = numpy.mean(dataM, axis=0)
sds = numpy.std(dataM, axis=0)
maxs = numpy.max(dataM, axis=0)
mins = numpy.min(dataM, axis=0)

colors = ["r","b","g","y","black"]

def plotContour(i, centroid):
    delta = 0.1
    x = numpy.arange(mins[0], maxs[0], delta)
    y = numpy.arange(mins[1], maxs[1], delta)
    X, Y = numpy.meshgrid(x, y)
    Z = mlab.bivariate_normal(X, Y, 1.0, 1.0, centroid[0], centroid[1])
    plt.contour(X, Y, Z, colors=colors[i])
    
kn = 5
centroids = [[numpy.random.normal(means[0],sds[0]),
              numpy.random.normal(means[1],sds[1])]
    for ki
    in range(0,kn)]

iterations = 1
for i in range(0,iterations):
    plt.scatter(dataM[:,0],dataM[:,1])
    for x in range(0,kn):
        plotContour(x, centroids[x])
        plt.savefig("state_" + "{:0>2}".format(i))    
print("ok")