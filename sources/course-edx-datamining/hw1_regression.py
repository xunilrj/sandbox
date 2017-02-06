#!/usr/bin/python

import os
os.path.dirname(os.path.abspath(__file__))

import sys
import csv
import numpy

l = 2
if(len(sys.argv) > 2):
    l = int(sys.argv[1])
    
s2 = 0
if(len(sys.argv) > 3):
    s2 = int(sys.argv[2])

xtrain = "X_train.csv"
if(len(sys.argv) > 4):
    xtrain = sys.argv[3]

ytrain = "y_train.csv"
if(len(sys.argv) > 5):
    ytrain = sys.argv[4]
    
xtest = "X_test.csv"
if(len(sys.argv) > 6):
    xtest = sys.argv[5]
    
print("lambda:", l)
print("sigma2:", s2)
print("xtrain:", xtrain)
print("ytrain:", ytrain)
print("xtest:", xtest)


Xmatrix = numpy.loadtxt(open(xtrain, "rb"), delimiter=",", skiprows=0)
XmatrixT = numpy.transpose(Xmatrix)
Ymatrix = numpy.loadtxt(open(ytrain, "rb"), delimiter=",", skiprows=0)

xTx = numpy.multiply(XmatrixT, Xmatrix)
lambdaMatrix = l *  numpy.identity(xTx.shape[0])

tobeInverted = lambdaMatrix + xTx
inverted = numpy.linalg.inv(tobeInverted)
wrr = numpy.multiply(numpy.multiply(inverted, XmatrixT), Ymatrix)



    
print("Argument List:", str(sys.argv))