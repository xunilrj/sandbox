#!/usr/bin/python

import os
os.path.dirname(os.path.abspath(__file__))

import sys
import numpy

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

XtrainM = numpy.loadtxt(open(xtrain, "rb"), delimiter=",", skiprows=0)
YtrainM = numpy.loadtxt(open(ytrain, "rb"), delimiter=",", skiprows=0)
XtestM = numpy.loadtxt(open(xtest, "rb"), delimiter=",", skiprows=0)

#class prior probabilities
numpy.unique(YtrainM)