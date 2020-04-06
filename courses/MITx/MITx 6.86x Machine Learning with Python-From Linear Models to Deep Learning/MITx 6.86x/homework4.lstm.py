
import math
import numpy as np

def sigmoid(x):
    if (x>=1):
        return 1
    if (x<=-1):
        return 0
    return 1 / (1 + np.exp(-x))

def tanh(x):
    if (x>=1):
        return 1
    if (x<=-1):
        return -1
    return np.tanh(x)

def ReLU(x):
    if (x <= 0):
        return 1
    return x

def lstm(xt):
    Wfh = 0
    Wfx = 0
    Wih = 0
    Wix = 100
    Woh = 0
    Wox = 100
    bf = -100
    bi = 100
    bo = 0
    bc = 0
    Wch = -100
    Wcx = 50
    htminus1 = 0
    ctminus1 = 0
    for i in range(xt.shape[0]):
        ft = sigmoid(Wfh*htminus1 + Wfx*xt[i] + bf)
        it = sigmoid(Wih*htminus1 + Wix*xt[i] + bi)
        ot = sigmoid(Woh*htminus1 + Wox*xt[i] + bo)
        ct = ft*ctminus1 + it*np.tanh(Wch*htminus1 + Wcx*xt[i] + bc)
        ht = ot*np.tanh(ct)
        ht = round(ht)
        print(ht)
        htminus1 = ht
        ctminus1 = ct

# LSTM 1
print("LSTM 1")
lstm(np.array([0,0,1,1,1,0]))

# LSTM 2
print("LSTM 2")
lstm(np.array([1,1,0,1,1]))

# LSTM Info
print("LSTM Info")
lstm(np.array([1,1,0,1]))
print("LSTM Info")
lstm(np.array([1,0,1,1]))

# Simple Network

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def sigmoid_d(x):
    return np.exp(-x) / (1 + np.exp(-x))**2

def tanh(x):
    return np.tanh(x)

def ReLU(x):
    return max(0,x)

def feed_fw(x,t, w1=0.01, w2=-5, b=-1):
    # print("w1",w1)
    # print("w2",w2)
    # print("b",w2)
    z1 = w1*x
    a1 = ReLU(z1)
    z2 = w2*a1 + b
    y = sigmoid(z2)
    C = 1/2 * (y-t)**2
    
    dCdw1 = (y-t) * sigmoid_d(z2) * w2 * x
    if(z1 <= 0):
        dCdw1 = 0
    dCdw2 = (y-t) * sigmoid_d(z2) * a1
    dCdb = (y-t) * sigmoid_d(z2)
    # print("dC/dw1",dCdw1)
    # print("dC/dw2",dCdw2)
    # print("dC/db",dCdb)
    return {
        "C": C,
        "dC/dw1":dCdw1,
        "dC/dw2":dCdw2,
        "dC/db":dCdb
    }

r = feed_fw(3, 1)
rw1 = feed_fw(3, 1, w1=1.01)
rw2 = feed_fw(3, 1, w2=-4)
rb = feed_fw(3, 1, b=0)

print("w1 increase. Expected:", r["dC/dw1"], " got:",  rw1["C"] - r["C"]) # why this difference so big? Expected is the correct
print("w2 increase. Expected:", r["dC/dw2"], " got:",  rw2["C"] - r["C"])
print("b increase. Expected:", r["dC/db"], " got:",  rb["C"] - r["C"])
