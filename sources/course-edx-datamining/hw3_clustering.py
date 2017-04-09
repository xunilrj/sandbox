#!/usr/bin/python

import os
os.path.dirname(os.path.abspath(__file__))

import sys
import numpy

data = "X_train.2.csv"
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
from scipy.stats import multivariate_normal


dataM = numpy.loadtxt(open(data, "rb"), delimiter=",", skiprows=0)
printMatrixShape("dataM", dataM)

means = numpy.mean(dataM, axis=0)
sds = numpy.std(dataM, axis=0)
maxs = numpy.max(dataM, axis=0)
mins = numpy.min(dataM, axis=0)

colors = ["r","g","b","y","black"]

def plotContour(i, centroid, params):
    delta = 0.1
    x = numpy.arange(mins[0], maxs[0], delta)
    y = numpy.arange(mins[1], maxs[1], delta)
    X, Y = numpy.meshgrid(x, y)
    Z = mlab.bivariate_normal(X, Y,
          params[0], params[1],
          centroid[0], centroid[1])
    plt.contour(X, Y, Z, colors=colors[i])
    
n = numpy.shape(dataM)[0]
d = numpy.shape(dataM)[1]
kn = 3
def getRandCentroid():
    return [numpy.random.normal(means[i],sds[i]) for i in range(0,d)]

centroids = [getRandCentroid()
    for ki
    in range(0,kn)]
params = [numpy.ones(d) for ki in range(0,kn)]

def xi(i):
    return numpy.reshape(numpy.matrix(dataM[i,:]), (1,d))
def likelihood(xi,means,sds):
    return multivariate_normal(means,sds).pdf(xi)
def pxigivenk(i,k):
    return likelihood(xi(i),centroids[k],params[k])
def pofk(k):
    return 1.0/kn;
def pkgivenxi(i,k):
    numerator = pxigivenk(i,k)*pofk(k)
    denominator = numpy.sum([pxigivenk(i,ck)*pofk(ck)
        for ck 
        in range(0,kn)])
    return numerator/denominator
def clusterColors():
    return [
        numpy.reshape(numpy.matrix([1.0,0.0,0.0]),(1,3)),
        numpy.reshape(numpy.matrix([0.0,1.0,0.0]),(1,3)),
        numpy.reshape(numpy.matrix([0.0,0.0,1.0]),(1,3))]
def clusterProbColors(probs):
    sums = numpy.sum(probs)
    colors = clusterColors()
    result = numpy.sum([(probs[i]/sums)*colors[i] for i in range(0,kn)],0) 
    return result[0]
def pointsColor(dataM,cM):    
    return [clusterProbColors(cM[i,:]) for i in range(0,n)]
    
iterations = 50
for i in range(0,iterations):
    print(str(i))
    cM = numpy.zeros((n,kn))
    for row in range(0,n):
        for k in range(0,kn):
            cM[row,k] = pkgivenxi(row,k)
    ksums = numpy.sum(cM, axis=0)    
    plt.figure()
    plt.scatter(dataM[:,0],dataM[:,1], c=pointsColor(dataM,cM))
    for k in range(0,kn):
        plotContour(k, centroids[k], params[k])
    plt.savefig("state_" + "{:0>2}".format(i))    
    for k in range(0,kn):
        for di in range(0,d):
            result = numpy.sum(dataM[:,di]*cM[:,k])/ksums[k]
            centroids[k][di] = result
            params[k][di] = numpy.sum((dataM[:,di]-result)*(dataM[:,di]-result)*cM[:,k])/ksums[k]    
print("ok")