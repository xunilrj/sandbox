
import sys
import pandas as pd
import numpy as np
   
inputfile = sys.argv[1]
outputfile = sys.argv[2]

#Perceptron Algorithm
#Input: A set of examples, (x1,y1),···,(xn,yn)
#Output: A perceptron deﬁned by (w0,w1,···,wd)
#Begin 2. Initialize the weights wj to 0 8j 2{0,···,d}
#3. Repeat until convergence
#4. For each example xi 8i 2{1,···,n}
#5. if yif(xi)  0 #an error?
#6. update all wj with wj := wj +yixi #adjust the weights End
def perceptron(df, features, label, file):
    wlen = len(features)
    weigths = np.repeat(0, wlen) 
    for iteration in range(1000): 
        keepiteration = False
        for index, row in df.iterrows():
            yi = row[label]
            xs = row[features]
            fxi = np.sum(xs*weigths)
            if(yi*fxi <= 0):
                keepiteration = True
                weigths = weigths + (yi*xs)
        #plot
        #colors = {1:'red', -1:'blue'}
        #plot = df.plot.scatter(x="feature1",y="feature2",c=df["label"].apply(lambda x: colors[x]))
        #slope = -(weigths[0]/weigths[2])/(weigths[0]/weigths[1])  
        #intercept = -weigths[0]/weigths[2]
        #line = slope*np.linspace(-100, 100, 100)+intercept
        #x_line = np.linspace(-100, 100, 100)
#       p1x = 0
#       p1y = -weigths[0]/weigths[2]
#       p2x = -weigths[0]/weigths[1]
#       p2y = 0
        #plot.plot(x_line, line)
        #plot.set_xlim([-5, 20])
        #plot.set_ylim([-25, 25])
        #figure = plot.get_figure()
        #figure.savefig("figure%04d.png" % iteration)
        file.write("%f,%f,%f\n" % (weigths[1],weigths[2],weigths[0]))
        if keepiteration == False:
            break
    return weigths      
  
df = pd.read_csv(inputfile, header = None, names = ["feature1","feature2","label"])
df["_one"] = np.repeat(1, len(df))
with open(outputfile,"w") as file:
    perceptron(df, ["_one", "feature1","feature2"],"label", file)


