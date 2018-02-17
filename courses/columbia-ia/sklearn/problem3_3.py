
import sys
import pandas as pd
#import numpy as np
from sklearn import svm
from sklearn.linear_model import LogisticRegression
from sklearn.cross_validation import train_test_split
from sklearn.neighbors import KNeighborsClassifier
from sklearn.grid_search import GridSearchCV
from sklearn import tree
from sklearn.ensemble import RandomForestClassifier
#import matplotlib.pyplot as plt

#import matplotlib as mpl
#import matplotlib.pyplot as plt
#from mpl_toolkits.mplot3d import Axes3D

inputfile = "input3.csv"
try:
    inputfile = sys.argv[1]
except:
    inputfile = "input3.csv"
outputfile = "output3.csv"
try:
    outputfile = sys.argv[2]
except:
    outputfile = "output3.csv"  
    
df = pd.read_csv(inputfile)
dftrain, dftest = train_test_split(df, test_size = 0.4, stratify=None)

#def runSVM(kernel, c, gamma):
#    clf = svm.SVC(kernel=kernel,C=c,gamma=gamma)
#    model = clf.fit(dftrain[["A","B"]].as_matrix(), dftrain["label"].as_matrix())
#    score = model.score(dftrain[["A","B"]].as_matrix(), dftrain["label"].as_matrix())
#    print(score)
#    amin = df["A"].min()
#    amax = df["A"].max()
#    bmin = df["B"].min()
#    bmax = df["B"].max()
#    h = .02
#    xx, yy = np.meshgrid(np.arange(amin, amax, h), np.arange(bmin, bmax, h))
#    zz = clf.predict(np.c_[xx.ravel(), yy.ravel()])
#    print(zz)
#    zz = zz.reshape(xx.shape)
#    plt.contourf(xx, yy, zz, cmap=plt.cm.coolwarm, alpha=0.8)
#    plt.scatter(dftest["A"], dftest["B"], c=dftest["label"], cmap=plt.cm.coolwarm)
#    plt.xlabel('Sepal length')
#    plt.ylabel('Sepal width')
#    plt.xlim(xx.min(), xx.max())
#    plt.ylim(yy.min(), yy.max())
#    plt.show()
    
def runLinear(file):
    parameters = [
      {'C': [0.1,0.5,1,5,10,50,100], 'kernel': ['linear']}
    ]
    classifier = svm.SVC()
    grid_search = GridSearchCV(classifier, parameters, cv=5, scoring='accuracy')
    grid_search.fit(dftrain[["A","B"]].as_matrix(), dftrain["label"].as_matrix())
    score = grid_search.score(dftest[["A","B"]].as_matrix(),dftest["label"].as_matrix())
    file.write("svm_linear,%f,%f\n" % (grid_search.best_score_,score))
def runPoly(file):
    parameters = [
      {'C': [0.1,1,3], 'gamma':[0.1,0.5,1,3,6,10], 'kernel': ['poly']}
    ]
    classifier = svm.SVC()
    grid_search = GridSearchCV(classifier, parameters, cv=5, scoring='accuracy')
    grid_search.fit(dftrain[["A","B"]].as_matrix(), dftrain["label"].as_matrix())
    score = grid_search.score(dftest[["A","B"]].as_matrix(),dftest["label"].as_matrix())
    file.write("svm_polynomial,%f,%f\n" % (grid_search.best_score_,score))
def runRBF(file):
    parameters = [
      {'C': [0.1, 0.5, 1, 5, 10, 50, 100], 'gamma':[0.1, 0.5, 1, 3, 6, 10], 'kernel': ['rbf']}
    ]
    classifier = svm.SVC()
    grid_search = GridSearchCV(classifier, parameters, cv=5, scoring='accuracy')
    grid_search.fit(dftrain[["A","B"]].as_matrix(), dftrain["label"].as_matrix())
    score = grid_search.score(dftest[["A","B"]].as_matrix(),dftest["label"].as_matrix())
    file.write("svm_rbf,%f,%f\n" % (grid_search.best_score_,score))
def runLogisticRegression(file):
    parameters = [
      {'C':  [0.1, 0.5, 1, 5, 10, 50, 100]}
    ]
    classifier = LogisticRegression()
    grid_search = GridSearchCV(classifier, parameters, cv=5, scoring='accuracy')
    grid_search.fit(dftrain[["A","B"]].as_matrix(), dftrain["label"].as_matrix())
    score = grid_search.score(dftest[["A","B"]].as_matrix(),dftest["label"].as_matrix())
    file.write("logistic,%f,%f\n" % (grid_search.best_score_,score))
def runKnn(file):
    parameters = [
      {'n_neighbors': list(range(1,51,1)), 'leaf_size': list(range(5,61,5))}
    ]
    classifier = KNeighborsClassifier()
    grid_search = GridSearchCV(classifier, parameters, cv=5, scoring='accuracy')
    grid_search.fit(dftrain[["A","B"]].as_matrix(), dftrain["label"].as_matrix())
    score = grid_search.score(dftest[["A","B"]].as_matrix(),dftest["label"].as_matrix())
    file.write("knn,%f,%f\n" % (grid_search.best_score_,score))
def runDecisionTree(file):
    parameters = [
      {'max_depth': list(range(1,51,1)), 'min_samples_split': [2, 3, 4, 5, 6, 7, 8, 9, 10]}
    ]
    classifier = tree.DecisionTreeClassifier()
    grid_search = GridSearchCV(classifier, parameters, cv=5, scoring='accuracy')
    grid_search.fit(dftrain[["A","B"]].as_matrix(), dftrain["label"].as_matrix())
    score = grid_search.score(dftest[["A","B"]].as_matrix(),dftest["label"].as_matrix())
    file.write("decision_tree,%f,%f\n" % (grid_search.best_score_,score))
def runRandomForest(file):
    parameters = [
      {'max_depth': list(range(1,51,1)), 'min_samples_split': [2, 3, 4, 5, 6, 7, 8, 9, 10]}
    ]
    classifier = RandomForestClassifier()
    grid_search = GridSearchCV(classifier, parameters, cv=5, scoring='accuracy')
    grid_search.fit(dftrain[["A","B"]].as_matrix(), dftrain["label"].as_matrix())
    score = grid_search.score(dftest[["A","B"]].as_matrix(),dftest["label"].as_matrix())
    file.write("random_forest,%f,%f\n" % (grid_search.best_score_,score))
with open(outputfile,"w") as file:
    runLinear(file)
    runPoly(file)
    runRBF(file)
    runLogisticRegression(file)
    runKnn(file)
    runDecisionTree(file)
    runRandomForest(file)
    
#possiblecs = 
#gammas = [0.1,0.5]
#for c in possiblecs:
#    runSVM('linear',c,'auto')
#for c in possiblecs:
#    for gamma in gammas:
#        runSVM('poly',c,gamma)
#for c in possiblecs:
#    for gamma in gammas:
#        runSVM('rbf',c,gamma)
    
