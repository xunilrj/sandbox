import numpy as np
import kmeans
import common
import naive_em
import em
import cProfile
import os
cwd = os.getcwd()

# X = np.loadtxt("toy_data.txt")

# for k in [1,2,3,4]:
#     best_mixture = None
#     best_post = None
#     best_cost = 9999999999999999
#     for seed in [0,1,2,3,4]:
#         X = np.loadtxt("toy_data.txt")
#         mixture, post = common.init(X, k, seed)
#         mixture, post, cost = kmeans.run(X, mixture, post)
#         if(cost < best_cost):
#             best_mixture = mixture
#             best_post = post
#     common.plot(X, mixture, post, "K=%d" % k)
#     print(k, cost)

# for k in [1,2,3,4]:
#     best_mixture = None
#     best_post = None
#     best_cost = 9999999999999999
#     for seed in [0,1,2,3,4]:
#         X = np.loadtxt("toy_data.txt")
#         mixture, post = common.init(X, k, seed)
#         mixture, post, cost = naive_em.run(X, mixture, post)
#         if(cost < best_cost):
#             best_mixture = mixture
#             best_post = post
#     common.plot(X, mixture, post, "K=%d" % k)
#     print(k, cost)
#     bic = common.bic(X, mixture, cost)
#     print(bic)

def run_k(args):
    k = args[0]
    seed = args[1]
    print("Seed%d-K=%d" % (seed, k))
    print('loading/', end='', flush=True)
    X = np.loadtxt("netflix_incomplete.txt")
    #X = np.loadtxt("toy_data.txt")
    print('initializing/', end='', flush=True)
    mixture, post = common.init(X, k, seed)
    print('running/', end='', flush=True)
    mixture, post, cost = em.run(X, mixture, post)
    print("")
    np.savetxt("Mixture.%d.%d.mu.txt" % (k, seed), mixture.mu)
    np.savetxt("Mixture.%d.%d.var.txt" % (k, seed), mixture.var)
    np.savetxt("Mixture.%d.%d.p.txt" % (k, seed), mixture.p)
    print(k, seed, mixture, cost)
    return (k, seed, mixture, cost)

import multiprocessing as mp
def run():
    #print("pool", mp.cpu_count())
    #pool = mp.Pool(mp.cpu_count())
    #results = pool.map(run_k, [[12, seed] for seed in [0,1,2,3,4]])
    #pool.close()   
    #print(results) 
    X = np.loadtxt("netflix_incomplete.txt")

    mu = np.loadtxt("Mixture.12.1.mu.txt")
    var = np.loadtxt("Mixture.12.1.var.txt")
    p = np.loadtxt("Mixture.12.1.p.txt")
    m = common.GaussianMixture(mu, var, p)

    Y = em.fill_matrix(X, m)
    rmse = common.rmse(X, Y)
    print(rmse)


#cProfile.run('run()')
run()

# K=3, seed=0, log-likelyhood=-1519326.0115300715, BIC=-1532105.8750265578, RMSE=0.4861648463577099
# K=3, seed=1, log-likelyhood=-1450478.9628848939, BIC=-1463258.8263813802, RMSE=0.4563254321834351
# K=3, seed=2, log-likelyhood=-1450611.488852833, BIC=-1463391.3523493193, RMSE=0.459588762027565
# K=3, seed=3, log-likelyhood=-1450442.8251735817, BIC=-1463222.688670068, RMSE=0.4606582154400679
# K=3, seed=4, log-likelyhood=-1450498.182808137, BIC=-1463278.0463046234, RMSE=0.46406940652629125
# K=3, max log-likelihood=-1450442.8251735817

#[( 1, 0, -1521060.9539852475), ( 1, 1, -1521060.9539852475), ( 1, 2, -1521060.9539852475), ( 1, 3, -1521060.9539852475), ( 1, 4, -1521060.9539852475)]
#-1521060.9539852475
#[( 3, 0, -1519326.0115300713), ( 3, 1, -1450478.9628848939), ( 3, 2, -1450611.488852833),  ( 3, 3, -1450442.8251735817), ( 3, 4, -1450498.1828081368)]
#[(12, 0, -1399803.0466569136), (12, 1, -1390234.4223469393), (12, 2, -1416862.4011512795), (12, 3, -1393521.3929897752), (12, 4, -1416733.8083763556)]
#-1390234.4223469393