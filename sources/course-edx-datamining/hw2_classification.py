#!/usr/bin/python

import os
os.path.dirname(os.path.abspath(__file__))

import sys
import numpy
import scipy


xtrain = "X_train.csv"
if(len(sys.argv) >= 2):
    xtrain = sys.argv[1]

ytrain = "y_train.csv"
if(len(sys.argv) >= 3):
    ytrain = sys.argv[2]
    
xtest = "X_test.csv"
if(len(sys.argv) >= 4):
    xtest = sys.argv[3]
    
print("xtrain:", xtrain)
print("ytrain:", ytrain)
print("xtest:", xtest)

from functools import reduce

XtrainM = numpy.loadtxt(open(xtrain, "rb"), delimiter=",", skiprows=0)
YtrainM = numpy.loadtxt(open(ytrain, "rb"), dtype=numpy.int64, delimiter=",", skiprows=0)
XtestM = numpy.loadtxt(open(xtest, "rb"), delimiter=",", skiprows=0)
def ml(indices):
    xk = XtrainM[indices]
    xksize = numpy.size(xk, axis=0)
    muhatml = numpy.mean(xk, axis=0)
    def sigmahatmlsummationitem(xi):
        ximinusmuhatml = numpy.matrix(xi - muhatml, )
        return numpy.dot(numpy.transpose(ximinusmuhatml),ximinusmuhatml)
    def sigmahatml():
        items = [sigmahatmlsummationitem(xk[i]) for i in range(0,xksize)]
        return numpy.sum(items, 0) * (1/xksize)
    return [muhatml,sigmahatml()]

#class prior probabilities
Ycount = numpy.size(YtrainM)
classes = numpy.unique(YtrainM)
pihat = numpy.divide(numpy.bincount(YtrainM), Ycount)

#class specific Gaussian parameters
classes = [ml(numpy.where(YtrainM == classk)) for classk in classes]
classesLen = len(classes)

#predict
from scipy.stats import multivariate_normal
def pdf(x):
    return [multivariate_normal(classes[k][0],classes[k][1]).pdf(x)
            for k in range(0, classesLen)]

probabilities = [pdf(XtestM[i]) for i in range(0,numpy.size(XtestM, axis=0))]
probabilitiesLen = len(probabilities)

        
def printMatrix(M):
    shape = numpy.shape(M)
    for row in range(0, shape[0]):
        for col in range(0, shape[1]):
            f.write('%.5f' % M[row][col])
            if(col != classesLen - 1):
                f.write(",") 
        f.write("," + str(numpy.array(M[row]).argmax()))    
        f.write("\n")

f = open("probs_test.csv", 'w')
print("mu")
print(classes[0][0])
print(classes[1][0])
print(classes[2][0])
print("sigma")
print(classes[0][1])
print(classes[1][1])
print(classes[2][1])
printMatrix(probabilities)
f.close()