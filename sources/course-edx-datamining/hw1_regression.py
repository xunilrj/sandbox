#!/usr/bin/python

import os
os.path.dirname(os.path.abspath(__file__))

import sys
import numpy
from sklearn.preprocessing import StandardScaler

l = 2
if(len(sys.argv) > 2):
    l = int(sys.argv[1])
    
s2 = 1
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

#WRR

Xmatrix = numpy.loadtxt(open(xtrain, "rb"), delimiter=",", skiprows=0)
#Xmatrix = StandardScaler().fit_transform(Xmatrix)
XmatrixT = numpy.transpose(Xmatrix)
Ymatrix = numpy.loadtxt(open(ytrain, "rb"), delimiter=",", skiprows=0)
#Ymatrix = Ymatrix - numpy.average(Ymatrix)

xTx = numpy.dot(XmatrixT,Xmatrix)
lambdaMatrix = l *  numpy.identity(xTx.shape[0])

tobeInverted = numpy.add(lambdaMatrix,xTx)
inverted = numpy.linalg.inv(tobeInverted)
wrr = numpy.dot(numpy.dot(inverted, XmatrixT), Ymatrix)

#ACTIVE

XtextM = numpy.loadtxt(open(xtest, "rb"), delimiter=",", skiprows=0)

ybar = numpy.mean(Ymatrix)
print(ybar)
predictions = numpy.dot(XtextM,wrr)

for row in range(0,numpy.shape(xTextM)[0]):
    predictions[row]-ybar

f = open('wRR_' + str(l) + ".csv", 'w')
for i in range(0, wrr.shape[0]):
    f.write(str(wrr[i]) + "\n")
f.close()

f = open('active_' + str(l) + "_" + str(s2) + ".csv", 'w')
for i in range(0, 9):
    f.write(str(si2sdesc[i][0]) + ",")
f.write(str(si2sdesc[9][0]))
f.close()