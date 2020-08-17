"""Mixture model using EM"""
from typing import Tuple
import numpy as np
from common import GaussianMixture
import math

from scipy.stats import multivariate_normal

def Gaussian(X, mu, var):
    return multivariate_normal.pdf(X, mean=mu, cov=np.eye(mu.shape[0])*var)

def estep(X: np.ndarray, mixture: GaussianMixture) -> Tuple[np.ndarray, float]:
    """E-step: Softly assigns each datapoint to a gaussian component

    Args:
        X: (n, d) array holding the data
        mixture: the current gaussian mixture

    Returns:
        np.ndarray: (n, K) array holding the soft counts
            for all components for all examples
        float: log-likelihood of the assignment
    """
    softcounts = np.empty((X.shape[0], mixture.mu.shape[0]))
    
    for k in range(mixture.mu.shape[0]):
        l = Gaussian(X, mixture.mu[k], mixture.var[k])
        softcounts[:,k] = mixture.p[k]*l

    denominator = np.sum(softcounts, axis=1)
    log_likelihood = np.sum(np.log(denominator))

    denominator = denominator.repeat(mixture.mu.shape[0]).reshape((X.shape[0], mixture.mu.shape[0]))
    return (softcounts/denominator, log_likelihood)


def mstep(X: np.ndarray, post: np.ndarray) -> GaussianMixture:
    """M-step: Updates the gaussian mixture by maximizing the log-likelihood
    of the weighted dataset

    Args:
        X: (n, d) array holding the data
        post: (n, K) array holding the soft counts
            for all components for all examples

    Returns:
        GaussianMixture: the new gaussian mixture
    """
    new_mus = np.empty((post.shape[1], X.shape[1]))
    new_vars = np.empty((post.shape[1], ))
    new_ps = np.empty((post.shape[1], ))
    
    denominator = np.sum(post, axis=0)

    for k in range(post.shape[1]):
        new_ps[k] = denominator[k] / X.shape[0]
        probs = np.repeat(post[:,k], X.shape[1]).reshape(post.shape[0], X.shape[1])
        new_mus[k,:] = np.sum(X*probs, axis=0) / denominator[k]
        x = np.sum((X-new_mus[k,:])**2*probs / denominator[k], axis=0)
        new_vars[k] = np.mean(x)
    
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
        post, new_log_likelihood = estep(X, mixture)
        mixture = mstep(X, post)

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
