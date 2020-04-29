"""Mixture model for matrix completion"""
from typing import Tuple
import numpy as np
from scipy.special import logsumexp
from common import GaussianMixture

from scipy.special import logsumexp
from scipy.stats import multivariate_normal
def log_pdf_multivariate_gauss(x, mu, cov):
    #assert(mu.shape[0] > mu.shape[1]), 'mu must be a row vector'
    #assert(x.shape[0] > x.shape[1]), 'x must be a row vector'
    #assert(cov.shape[0] == cov.shape[1]), 'covariance matrix must be square'
    #assert(mu.shape[0] == cov.shape[0]), 'cov_mat and mu_vec must have the same dimensions'
    #assert(mu.shape[0] == x.shape[0]), 'mu and x must have the same dimensions'
    
    logdet = (1/2)*len(mu)*np.log(cov)
    #invcov = 1/cov[0,0]*np.eye(len(mu)) # np.linalg.inv(cov)# cov/cov[0,0]
    xmu = x-mu
    
    part1 = np.log(1) - (len(mu)/2)*np.log(2* np.pi) - logdet
    #part2 = (-1/2) * xmu.T.dot(invcov).dot(xmu)
    part2 = (-1/2) * xmu.dot(xmu) / cov
    return part1 + part2
def pdf_multivariate_gauss(x, mu, cov):
    #assert(mu.shape[0] > mu.shape[1]), 'mu must be a row vector'
    #assert(x.shape[0] > x.shape[1]), 'x must be a row vector'
    assert(cov.shape[0] == cov.shape[1]), 'covariance matrix must be square'
    assert(mu.shape[0] == cov.shape[0]), 'cov_mat and mu_vec must have the same dimensions'
    assert(mu.shape[0] == x.shape[0]), 'mu and x must have the same dimensions'
    part1 = 1 / ( ((2* np.pi)**(len(mu)/2)) * (np.linalg.det(cov)**(1/2)) )
    part2 = (-1/2) * ((x-mu).T.dot(np.linalg.inv(cov))).dot((x-mu))
    return float(part1 * np.exp(part2))
def Gaussian(X, mu, var):
    return pdf_multivariate_gauss(X,mu,var)
    #return multivariate_normal.pdf(X, mean=mu, cov=np.eye(mu.shape[0])*var)


# from multiprocessing.dummy import Pool as ThreadPool
# pool = ThreadPool(8)
def estep(X: np.ndarray, mixture: GaussianMixture) -> Tuple[np.ndarray, float]:
    """E-step: Softly assigns each datapoint to a gaussian component

    Args:
        X: (n, d) array holding the data, with incomplete entries (set to 0)
        mixture: the current gaussian mixture

    Returns:
        np.ndarray: (n, K) array holding the soft counts
            for all components for all examples
        float: log-likelihood of the assignment

    """
    softcounts = np.zeros((X.shape[0], mixture.mu.shape[0]))

    nz = X != 0

    cus = {}
    for n in range(X.shape[0]):
        cus[n] = X[n, nz[n]]
    nzcount = np.count_nonzero(X, axis=1)
    
    for k in range(mixture.mu.shape[0]):
        softcounts[:,k] += np.log(mixture.p[k] + 1e-16)
        for n in range(X.shape[0]):
            if(nzcount[n] != 0):
                idx = nz[n]          
                row = cus[n]
                rowmu = mixture.mu[k, idx]
                #rowvar = mixture.var[k]*np.eye(row.shape[0])            
                rowvar = mixture.var[k]
                l = log_pdf_multivariate_gauss(row, rowmu, rowvar)
                softcounts[n,k] += l 
    
    denominator = logsumexp(softcounts, axis=1) 
    log_likelihood = np.sum(denominator)    
    
    denominator = denominator.repeat(mixture.mu.shape[0]).reshape((X.shape[0], mixture.mu.shape[0]))
    post = np.exp(softcounts-denominator)
    
    return (post, log_likelihood)



def mstep(X: np.ndarray, post: np.ndarray, mixture: GaussianMixture,
          min_variance: float = .25) -> GaussianMixture:
    """M-step: Updates the gaussian mixture by maximizing the log-likelihood
    of the weighted dataset

    Args:
        X: (n, d) array holding the data, with incomplete entries (set to 0)
        post: (n, K) array holding the soft counts
            for all components for all examples
        mixture: the current gaussian mixture
        min_variance: the minimum variance for each gaussian

    Returns:
        GaussianMixture: the new gaussian mixture
    """
    new_mus = np.zeros((post.shape[1], X.shape[1]))
    new_mus_d = np.zeros((post.shape[1], X.shape[1]))
    new_vars = np.zeros((post.shape[1], ))
    new_ps = np.zeros((post.shape[1], ))

    nz = X != 0
    zero_one_by_l = np.where(X != 0, 1, 0)

    for k in range(post.shape[1]):
        new_vars[k] = 0
        post_all_k = post[:,k]
        for l in range(X.shape[1]):   
            prob_cu = post_all_k * zero_one_by_l[:,l]
            new_mus_d[k,l] += np.sum(prob_cu)
            if (new_mus_d[k,l] >= 1):
                x_all_l = X[:,l]
                new_mus[k,l] = np.sum(prob_cu * x_all_l) / new_mus_d[k,l]
            else:
                new_mus[k,l] = mixture.mu[k,l] 
        denominator = 0
        new_vars[k] = 0
        for u in range(X.shape[0]):
            denominator += np.count_nonzero(X[u,:]) * post[u,k]
            w = zero_one_by_l[u,:] #np.where(X[u,:] != 0, 1, 0)
            new_vars[k] += np.sum(post[u,k] * w * (X[u,:] - new_mus[k,:])**2)
        new_vars[k] /= denominator
        if(new_vars[k] < 0.25): new_vars[k] = 0.25
        new_ps[k] += np.sum(post_all_k) / X.shape[0]

    return GaussianMixture(new_mus, new_vars, new_ps)


def run(X: np.ndarray, mixture: GaussianMixture,
        post: np.ndarray) -> Tuple[GaussianMixture, np.ndarray, float]:
    """Runs the mixture model

    Args:
        X: (n, d) array holding the data
        post: (n, K) array holding the soft counts
            for all components for all examples

    Returns:
        GaussianMixture: the new gaussian mixture
        np.ndarray: (n, K) array holding the soft counts
            for all components for all examples
        float: log-likelihood of the current assignment
    """
    #print(mixture)
    old_log_likelihood = None
    max_steps = 1000
    converged = False
    while((not converged) and (max_steps > 0)):
        print('e', end='', flush=True)
        post, new_log_likelihood = estep(X, mixture)
        print('m', end='', flush=True)
        mixture = mstep(X, post, mixture)

        if(old_log_likelihood == None):
            old_log_likelihood = new_log_likelihood
        else:
            diff = new_log_likelihood - old_log_likelihood
            margin = 1e-6 * np.abs(new_log_likelihood)
            #print(diff, margin)        
            if(diff <= margin):
                converged = True
        
        old_log_likelihood = new_log_likelihood
        max_steps -= 1
    return mixture, post, old_log_likelihood


def fill_matrix(X: np.ndarray, mixture: GaussianMixture) -> np.ndarray:
    """Fills an incomplete matrix according to a mixture model

    Args:
        X: (n, d) array of incomplete data (incomplete entries =0)
        mixture: a mixture of gaussians

    Returns
        np.ndarray: a (n, d) array with completed data
    """
    softcounts = np.zeros((X.shape[0], mixture.mu.shape[0]))

    nz = X != 0

    cus = {}
    for n in range(X.shape[0]):
        cus[n] = X[n, nz[n]]
    nzcount = np.count_nonzero(X, axis=1)
    
    for k in range(mixture.mu.shape[0]):
        softcounts[:,k] += np.log(mixture.p[k] + 1e-16)
        for n in range(X.shape[0]):
            if(nzcount[n] != 0):
                idx = nz[n]          
                row = cus[n]
                rowmu = mixture.mu[k, idx]
                #rowvar = mixture.var[k]*np.eye(row.shape[0])            
                rowvar = mixture.var[k]
                l = log_pdf_multivariate_gauss(row, rowmu, rowvar)
                softcounts[n,k] += l 
    
    denominator = logsumexp(softcounts, axis=1) 
    denominator = denominator.repeat(mixture.mu.shape[0]).reshape((X.shape[0], mixture.mu.shape[0]))
    post = np.exp(softcounts-denominator)

    Xstar = np.copy(X)
    for u in range(X.shape[0]):
        for l in range(X.shape[1]):
            if(X[u,l] == 0):
                Xstar[u,l] = np.sum(post[u,:] * mixture.mu[:,l])
            
    return Xstar
