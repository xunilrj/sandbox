#!/usr/bin/python3

out_path = "C:/github/xunilrj-sandbox/sources/courses/columbia-ia/sentiment/"
train_path = "C:/github/xunilrj-sandbox/sources/courses/columbia-ia/sentiment/aclImdb/train"
test_path = "C:/github/xunilrj-sandbox/sources/courses/columbia-ia/sentiment/imdb_te.csv"

import os
import pandas as pd

def remove_non_ascii(unicode_string):
     return unicode_string.encode('ascii', 'replace').decode().replace('"','""')

def readText(f):
    with open(f,"r", encoding = "ISO-8859-1") as handle:
        return handle.read()

def imdb_data_preprocess(inpath, outpath="./", name="imdb_tr.csv", mix=False):
    outfilename = os.path.join(outpath,name)
    rownumber = 0
    with open(outfilename,"w",  encoding = "ISO-8859-1") as outhandle:
        outhandle.write("rownumber,text,polarity")
        pospath = os.path.join(inpath,"pos")
        def getfullname(f):
            return os.path.join(pospath, f)
        files = [getfullname(f) for f in os.listdir(pospath) if os.path.isfile(getfullname(f))]
        for filename in files:
            text = readText(filename)
            outhandle.write("\n%s,\"%s\",%s" % (rownumber,remove_non_ascii(text),"1"))
            rownumber += 1
        #
        pospath = os.path.join(inpath,"neg")
        def getfullname(f):
            return os.path.join(pospath, f)
        files = [getfullname(f) for f in os.listdir(pospath) if os.path.isfile(getfullname(f))]
        for filename in files:
            text = readText(filename)
            outhandle.write("\n%s,\"%s\",%s" % (rownumber,remove_non_ascii(text),"0"))
            rownumber += 1

imdb_data_preprocess(train_path)
#load stop-words
stopwords = []
with open("stopwords.en.txt") as stopFile:
    for line in stopFile:
        stopwords.append(line.replace("\n",""))

def getDf(fileName):
    return pd.read_csv(fileName, encoding = "ISO-8859-1")

from queue import Queue 

def textArrayToWordsArray(lines, size = 1):
    def grams(lines):
        q = Queue()
        if(size > 1):
            q.put("<START>")
        words = [word for word in line.split(" ") if (word.lower() not in stopwords)]
        for word in words:
            q.put(word)
            if(q.qsize() == size):
                ngram = "_".join(list(q.queue))
                q.get()
                yield ngram
        if(size > 1):
            q.put("<END>")
            if(q.qsize() == size):
                ngram = "_".join(list(q.queue))
                q.get()
                yield ngram
    D = []
    for line in lines:
        D.append({gram:1 for gram in grams(lines)})
    return D

from sklearn.feature_extraction import DictVectorizer
from sklearn.linear_model import SGDClassifier
from sklearn.feature_extraction.text import TfidfTransformer

dftrain = getDf("./imdb_tr.csv")
dftest = getDf(test_path)
def runModel(outputFile, size, getVectorizer, getNormalizer = None):
    train_x = dftrain["text"].as_matrix()
    train_y = dftrain["polarity"].as_matrix()
    D = textArrayToWordsArray(train_x, size)
    vectorizer = getVectorizer()
    X = vectorizer.fit_transform(D)
    if(getNormalizer != None):
        normalizer = getNormalizer()
        X = normalizer.fit_transform(X)
    classifier = SGDClassifier(loss="hinge", penalty="l1")
    classifier.fit(X, train_y)
    test_x = dftest["text"].as_matrix()
    D = textArrayToWordsArray(test_x, size)
    X = vectorizer.transform(D)
    result = classifier.predict(X)
    with open(outputFile,"w") as f:
        for r in result:
            f.write("%s\n" % (r))        
runModel("unigram.output.txt", 1, lambda: DictVectorizer(sparse=True))
runModel("bigram.output.txt", 2, lambda: DictVectorizer(sparse=True))
runModel("unigramtfidf.output.txt", 1, lambda: DictVectorizer(sparse=True), lambda: TfidfTransformer())
runModel("bigramtfidf.output.txt", 2, lambda: DictVectorizer(sparse=True), lambda: TfidfTransformer())