#!/usr/bin/python

import os
os.path.dirname(os.path.abspath(__file__))

import sys
import numpy
from scipy.stats import multivariate_normal

xtrain = "X_train.csv"
if(len(sys.argv) >= 2):
    xtrain = sys.argv[1]

ytrain = "y_train.csv"
if(len(sys.argv) >= 3):
    ytrain = sys.argv[2]
    
xtest = "X_test.csv"
if(len(sys.argv) >= 4):
    xtest = sys.argv[3]

XtrainM = numpy.loadtxt(open(xtrain, "rb"), delimiter=",", skiprows=0)
YtrainM = numpy.loadtxt(open(ytrain, "rb"), dtype=numpy.int64, delimiter=",", skiprows=0)
XtestM = numpy.loadtxt(open(xtest, "rb"), delimiter=",", skiprows=0)

#class prior probabilities 
YtrainMCount = numpy.size(YtrainM)
def classpercentage(k):
    indices = numpy.where(YtrainM == k)
    yk = YtrainM[indices]
    count = numpy.size(yk)
    return (count * 1.00) / (YtrainMCount * 1.00)
pihat = {k:classpercentage(k) for k in numpy.unique(YtrainM)}

#class specific MLE estimate
def mle(k):
    indices = numpy.where(YtrainM == k)
    xk = XtrainM[indices]
    muhat = numpy.mean(xk, axis=0)
    centeredxk = xk - muhat
    sigmahat = numpy.dot(numpy.transpose(centeredxk), centeredxk)
    return [muhat, numpy.mean(sigmahat, axis=0)]

classes = numpy.unique(YtrainM) 
mlEstimates = {k:mle(k) for k in classes}
classesCount = len(mlEstimates)

def muhat(k):
    return mlEstimates[k][0]
def sigmahat(k):
    return mlEstimates[k][1]
def pofxgivenmuandsigma(x,mu,sigma):
    return multivariate_normal(mu,sigma).pdf(x)

#predict
def pforeachclass(i):
    return [pofxgivenmuandsigma(XtestM[i], muhat(k),sigmahat(k))*pihat[k]
            for k in classes]
def normalizepofeachclass(ps):
    #sumps = numpy.sum(ps)
    #return ps / sumps;
    return ps

XtestMRows = numpy.size(XtestM, axis=0)
probabilities = [normalizepofeachclass(pforeachclass(i)) for i in range(0,XtestMRows)]
probabilitiesLen = len(probabilities)
   
#print result file
def printMatrix(M):
    shape = numpy.shape(M)
    for row in range(0, shape[0]):
        for col in range(0, shape[1]):
            f.write('%.50f' % M[row][col])
            if(col != classesCount - 1):
                f.write(",")
        f.write("," + str(numpy.array(M[row]).argmax()))     
        f.write("\n")

f = open("probs_test.csv", 'w')
printMatrix(probabilities)
f.close()