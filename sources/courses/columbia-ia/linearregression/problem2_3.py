
import sys
import pandas as pd
import numpy as np
#import matplotlib as mpl
#import matplotlib.pyplot as plt
#from mpl_toolkits.mplot3d import Axes3D

inputfile = "input2.csv"
try:
    inputfile = sys.argv[1]
except:
    inputfile = "input2.csv"
outputfile = "output.csv"
try:
    outputfile = sys.argv[2]
except:
    outputfile = "output.csv"  
    
df = pd.read_csv(inputfile, header = None, names = ["age","weight","heigth"])

def normalize(df):
    result = df.copy()
    for feature_name in df.columns:
        max_value = df[feature_name].max()
        min_value = df[feature_name].min()
        result[feature_name] = (df[feature_name] - min_value) / (max_value - min_value)
    return result
def frange(start,end,step):
    return [x*step for x in range(start,int(end/step))]
def graddesc(df, features, value, alpha, size, startimage):
    def summfxi(df,features,value,beta):
        def fxi(betas,row):
            return np.sum(betas*row[features])
        values = [(fxi(beta,row)-row[value])*row[features] for (index,row) in df.iterrows()]
        return np.sum(values, axis=0)
    n = len(df)
    overn = 1/n
    betas = np.repeat(0, len(features))
    for i in range(size):
        betas = betas - alpha*overn*summfxi(df,features,value,betas)
#        fig = plt.figure()
#        ax = fig.add_subplot(111, projection='3d')
#        ax.scatter(df["age"], df["weight"], df["heigth"])
#        ax.set_xlabel('age')
#        ax.set_ylabel('weight')
#        ax.set_ylabel('heigth')
#        xx, yy = np.meshgrid(frange(0,1,0.1), frange(0,1,0.1))
#        z = (betas[1] * xx + betas[2] * yy + betas[0])
#        ax.plot_wireframe(xx, yy, z, alpha=0.2)
#        fig.savefig("figure%04d.png" % (i+startimage))
    return betas
    
df = normalize(df)
df["_one"] = np.repeat(1, len(df))
params = [(0.001,100), (0.005,100), (0.01,100), (0.05,100), (0.1,100), (0.5,100), (1,100), (5,100), (10,100),(1.25,300)]
with open(outputfile,"w") as file:
    i = 0
    for (r,size) in params:
        betas = graddesc(df,["_one","age","weight"],"heigth", r, size, i)
        file.write("%f,%d,%f,%f,%f\n" % (r,100,betas[0],betas[1],betas[2]))
        i = i + 100
        

