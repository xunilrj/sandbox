import numpy as np
import kmeans
import common
import naive_em
import em

import os
cwd = os.getcwd()

X = np.loadtxt("toy_data.txt")

for k in [1,2,3,4]:
    best_mixture = None
    best_post = None
    best_cost = 9999999999999999
    for seed in [0,1,2,3,4]:
        X = np.loadtxt("toy_data.txt")
        mixture, post = common.init(X, k, seed)
        mixture, post, cost = kmeans.run(X, mixture, post)
        if(cost < best_cost):
            best_mixture = mixture
            best_post = post
    common.plot(X, mixture, post, "K=%d" % k)
    print(k, cost)

for k in [1,2,3,4]:
    best_mixture = None
    best_post = None
    best_cost = 9999999999999999
    for seed in [0,1,2,3,4]:
        X = np.loadtxt("toy_data.txt")
        mixture, post = common.init(X, k, seed)
        mixture, post, cost = naive_em.run(X, mixture, post)
        if(cost < best_cost):
            best_mixture = mixture
            best_post = post
    common.plot(X, mixture, post, "K=%d" % k)
    print(k, cost)
    
