import numpy as np

### Functions for you to fill in ###



def polynomial_kernel(X, Y, c, p):
    """
        Compute the polynomial kernel between two matrices X and Y::
            K(x, y) = (<x, y> + c)^p
        for each pair of rows x in X and y in Y.

        Args:
            X - (n, d) NumPy array (n datapoints each with d features)
            Y - (m, d) NumPy array (m datapoints each with d features)
            c - a coefficient to trade off high-order and low-order terms (scalar)
            p - the degree of the polynomial kernel

        Returns:
            kernel_matrix - (n, m) Numpy array containing the kernel matrix
    """
    return np.power((X @ Y.T + c), p)



def rbf_kernel(X, Y, gamma):
    """
        Compute the Gaussian RBF kernel between two matrices X and Y::
            K(x, y) = exp(-gamma ||x-y||^2)
        for each pair of rows x in X and y in Y.

        Args:
            X - (n, d) NumPy array (n datapoints each with d features)
            Y - (m, d) NumPy array (m datapoints each with d features)
            gamma - the gamma parameter of gaussian function (scalar)

        Returns:
            kernel_matrix - (n, m) Numpy array containing the kernel matrix
    """
    n = X.shape[0]
    m = Y.shape[0]
    K = np.zeros((n,m))
    for i in range(n):
        for j in range(m):
            K[i,j] = np.exp(-gamma * (np.linalg.norm(X[i] - Y[j]) ** 2))
    return K
    # np.vectorize
    # n = X.shape[0]
    # m = Y.shape[0]
    # a = np.mat((X@X.T).diagonal())
    # a = np.tile(a.T, (1,m))

    # b = np.mat((Y@Y.T).diagonal())
    # b = np.tile(b, (n,1))

    # k = a + b
    # k -= 2*(X @ np.transpose(Y))

    # return np.exp(-gamma * k)
