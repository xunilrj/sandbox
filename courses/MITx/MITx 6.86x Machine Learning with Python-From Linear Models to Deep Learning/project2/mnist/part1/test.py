import os
import sys
import time
import traceback
import numpy as np
import linear_regression
import svm
import softmax
import features
import kernel

sys.path.append("..")
import utils

verbose = False

epsilon = 1e-6

def green(s):
    return '\033[1;32m%s\033[m' % s

def yellow(s):
    return '\033[1;33m%s\033[m' % s

def red(s):
    return '\033[1;31m%s\033[m' % s

def log(*m):
    print(" ".join(map(str, m)))

def log_exit(*m):
    log(red("ERROR:"), *m)
    exit(1)


def check_real(ex_name, f, exp_res, *args):
    try:
        res = f(*args)
    except NotImplementedError:
        log(red("FAIL"), ex_name, ": not implemented")
        return True
    if not np.isreal(res):
        log(red("FAIL"), ex_name, ": does not return a real number, type: ", type(res))
        return True
    if not -epsilon < res - exp_res < epsilon:
        log(red("FAIL"), ex_name, ": incorrect answer. Expected", exp_res, ", got: ", res)
        return True


def equals(x, y):
    if type(y) == np.ndarray:
        return (np.abs(x - y) < epsilon).all()
    return -epsilon < x - y < epsilon

def check_tuple(ex_name, f, exp_res, *args, **kwargs):
    try:
        res = f(*args, **kwargs)
    except NotImplementedError:
        log(red("FAIL"), ex_name, ": not implemented")
        return True
    if not type(res) == tuple:
        log(red("FAIL"), ex_name, ": does not return a tuple, type: ", type(res))
        return True
    if not len(res) == len(exp_res):
        log(red("FAIL"), ex_name, ": expected a tuple of size ", len(exp_res), " but got tuple of size", len(res))
        return True
    if not all(equals(x, y) for x, y in zip(res, exp_res)):
        log(red("FAIL"), ex_name, ": incorrect answer. Expected", exp_res, ", got: ", res)
        return True

def check_array(ex_name, f, exp_res, *args):
    try:
        res = f(*args)
    except NotImplementedError:
        log(red("FAIL"), ex_name, ": not implemented")
        return True
    if not type(res) == np.ndarray:
        log(red("FAIL"), ex_name, ": does not return a numpy array, type: ", type(res))
        return True
    if not len(res) == len(exp_res):
        log(red("FAIL"), ex_name, ": expected an array of shape ", exp_res.shape, " but got array of shape", res.shape)
        return True
    if not equals(res, exp_res):
        log(red("FAIL"), ex_name, ": incorrect answer. Expected", exp_res, ", got: ", res)

        return True

def check_list(ex_name, f, exp_res, *args):
    try:
        res = f(*args)
    except NotImplementedError:
        log(red("FAIL"), ex_name, ": not implemented")
        return True
    if not type(res) == list:
        log(red("FAIL"), ex_name, ": does not return a list, type: ", type(res))
        return True
    if not len(res) == len(exp_res):
        log(red("FAIL"), ex_name, ": expected a list of size ", len(exp_res), " but got list of size", len(res))
        return True
    if not all(equals(x, y) for x, y in zip(res, exp_res)):
        log(red("FAIL"), ex_name, ": incorrect answer. Expected", exp_res, ", got: ", res)
        return True

def check_get_mnist():
    ex_name = "Get MNIST data"
    train_x, train_y, test_x, test_y = utils.get_MNIST_data()
    log(green("PASS"), ex_name, "")


def check_closed_form():
    ex_name = "Closed form"
    X = np.arange(1, 16).reshape(3, 5)
    Y = np.arange(1, 4)
    lambda_factor = 0.5
    exp_res = np.array([-0.03411225,  0.00320187,  0.04051599,  0.07783012,  0.11514424])
    if check_array(
            ex_name, linear_regression.closed_form,
            exp_res, X, Y, lambda_factor):
        return

    log(green("PASS"), ex_name, "")

def check_svm():
    ex_name = "One vs rest SVM"
    n, m, d = 5, 3, 7
    train_x = np.random.random((n, d))
    test_x = train_x[:m]
    train_y = np.zeros(n)
    train_y[-1] = 1
    exp_res = np.zeros(m)

    if check_array(
            ex_name, svm.one_vs_rest_svm,
            exp_res, train_x, train_y, test_x):
        return

    train_y = np.ones(n)
    train_y[-1] = 0
    exp_res = np.ones(m)

    if check_array(
            ex_name, svm.one_vs_rest_svm,
            exp_res, train_x, train_y, test_x):
        return

    train_x = [
        [0.82909161, 0.80997416, 0.40144849],
        [0.17244945, 0.39108964, 0.69737362],
        [0.84956285, 0.45182649, 0.63594407],
        [0.78175441, 0.0254772,  0.88305236],
        [0.77986107, 0.17315278, 0.3858377 ],
        [0.2122153 , 0.40928021, 0.7565578 ],
        [0.94217178, 0.92300571, 0.955988  ],
        [0.78354919, 0.27897472, 0.67169795],
        [0.69978758, 0.26790694, 0.07953636],
        [0.88144119, 0.57226476, 0.09226887],
        [0.06253658, 0.99015304, 0.98302419]
    ]
    train_y = [0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0]
    test_x = [
        [0.76096788, 0.43391565, 0.15993481],
        [0.90527958, 0.55590257, 0.09619995],
        [0.580703  , 0.24759685, 0.30388295],
        [0.15453634, 0.66753221, 0.77578866],
        [0.02035136, 0.45553856, 0.80982499],
        [0.04085757, 0.26241906, 0.17495266],
        [0.12152709, 0.39147195, 0.60916317],
        [0.32473919, 0.92488444, 0.67298154],
        [0.66790052, 0.58783179, 0.67901959],
        [0.51550577, 0.6618569,  0.88648493]
    ]
    exp_res = np.array([1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    if check_array(
            ex_name, svm.one_vs_rest_svm,
            exp_res, train_x, train_y, test_x):
        return
    log(green("PASS"), ex_name, "")


def check_compute_probabilities():
    ex_name = "Compute probabilities"
    n, d, k = 3, 5, 7
    X = np.arange(0, n * d).reshape(n, d)
    zeros = np.zeros((k, d))
    temp = 0.2
    exp_res = np.ones((k, n)) / k
    if check_array(
            ex_name, softmax.compute_probabilities,
            exp_res, X, zeros, temp):
        return

    theta = np.arange(0, k * d).reshape(k, d)
    softmax.compute_probabilities(X, theta, temp)
    exp_res = np.zeros((k, n))
    exp_res[-1] = 1
    if check_array(
            ex_name, softmax.compute_probabilities,
            exp_res, X, theta, temp):
        return

    log(green("PASS"), ex_name, "")

def check_compute_cost_function():
    ex_name = "Compute cost function"
    n, d, k = 3, 5, 7
    X = np.arange(0, n * d).reshape(n, d)
    Y = np.arange(0, n)
    zeros = np.zeros((k, d))
    temp = 0.2
    lambda_factor = 0.5
    exp_res = 1.9459101490553135
    if check_real(
            ex_name, softmax.compute_cost_function,
            exp_res, X, Y, zeros, lambda_factor, temp):
        return

    X = np.array([
        [ 1., 98., 14., 94., 86., 47., 27., 10., 73.,  2., 46.],
        [ 1., 86., 76., 63., 50., 90., 10., 80., 57., 74., 30.],
        [ 1., 87., 14., 19., 84., 77., 23., 21., 74., 71., 52.],
        [ 1., 43., 42., 28., 60., 50., 69., 59., 25., 54., 92.],
        [ 1., 55., 67., 84., 55., 22., 25., 94., 87., 49., 71.],
        [ 1., 96., 82., 95., 80., 93., 60.,  6., 43., 32., 55.],
        [ 1., 15.,  5., 54., 37., 61.,  9., 43., 45., 95., 51.],
        [ 1., 65., 85., 50., 62., 80., 37., 29., 96., 16., 39.],
        [ 1., 71., 27., 98.,  7., 41., 29., 80., 39., 41., 84.],
        [ 1., 12., 99., 51., 17., 86., 65., 19., 70., 19., 40.]])
    Y = np.array([1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    theta = np.array([
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
        [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.]])
    temp = 1
    lambda_factor = 0.0001
    print(softmax.compute_cost_function(X,Y,theta, lambda_factor, temp))

    X = np.array([
        [ 1., 67., 86., 66., 52., 94., 57., 93., 68., 22., 78.], 
        [ 1., 90., 19., 23., 75., 91., 87., 48., 98., 56.,  9.],
        [ 1., 34., 11., 95., 79., 13., 67., 70., 78., 83., 53.],
        [ 1., 31., 57., 11.,  9.,  7., 76.,  6., 57., 61., 26.],
        [ 1., 77., 91., 31., 79., 62., 44., 79., 13., 17., 99.],
        [ 1., 38., 89., 36., 64., 79., 40., 16., 52., 19., 85.],
        [ 1., 66., 96., 42., 67., 67., 70., 37., 88., 75., 45.],
        [ 1.,  5., 13., 78., 61., 81., 36., 15., 51., 76., 76.],
        [ 1.,  5., 94., 89., 15., 52., 10., 56., 49., 68., 90.],
        [ 1., 76., 67., 70., 26., 90., 55., 17., 97., 60., 19.]
    ])
    Y = np.array([1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    theta = np.array([
        [-0.03 , -1.467, -1.869, -1.623, -1.581, -1.908, -1.626, -1.311, -1.953, -1.611, -1.74 ],
        [ 0.27 , 13.203, 16.821, 14.607, 14.229, 17.172, 14.634, 11.799, 17.577, 14.499, 15.66 ],
        [-0.03 , -1.467, -1.869, -1.623, -1.581, -1.908, -1.626, -1.311, -1.953, -1.611, -1.74 ],
        [-0.03 , -1.467, -1.869, -1.623, -1.581, -1.908, -1.626, -1.311, -1.953, -1.611, -1.74 ],
        [-0.03 , -1.467, -1.869, -1.623, -1.581, -1.908, -1.626, -1.311, -1.953, -1.611, -1.74 ],
        [-0.03 , -1.467, -1.869, -1.623, -1.581, -1.908, -1.626, -1.311, -1.953, -1.611, -1.74 ],
        [-0.03 , -1.467, -1.869, -1.623, -1.581, -1.908, -1.626, -1.311, -1.953, -1.611, -1.74 ],
        [-0.03 , -1.467, -1.869, -1.623, -1.581, -1.908, -1.626, -1.311, -1.953, -1.611, -1.74 ],
        [-0.03 , -1.467, -1.869, -1.623, -1.581, -1.908, -1.626, -1.311, -1.953, -1.611, -1.74 ],
        [-0.03 , -1.467, -1.869, -1.623, -1.581, -1.908, -1.626, -1.311, -1.953, -1.611, -1.74 ]
    ])
    temp = 1
    lambda_factor = 0.0001
    print(softmax.compute_cost_function(X,Y,theta, lambda_factor, temp))
    
    log(green("PASS"), ex_name, "")

def check_run_gradient_descent_iteration():
    ex_name = "Run gradient descent iteration"
    n, d, k = 3, 5, 7
    X = np.arange(0, n * d).reshape(n, d)
    Y = np.arange(0, n)
    zeros = np.zeros((k, d))
    alpha = 2
    temp = 0.2
    lambda_factor = 0.5
    exp_res = np.zeros((k, d))
    exp_res = np.array([
       [ -7.14285714,  -5.23809524,  -3.33333333,  -1.42857143, 0.47619048],
       [  9.52380952,  11.42857143,  13.33333333,  15.23809524, 17.14285714],
       [ 26.19047619,  28.0952381 ,  30.        ,  31.9047619 , 33.80952381],
       [ -7.14285714,  -8.57142857, -10.        , -11.42857143, -12.85714286],
       [ -7.14285714,  -8.57142857, -10.        , -11.42857143, -12.85714286],
       [ -7.14285714,  -8.57142857, -10.        , -11.42857143, -12.85714286],
       [ -7.14285714,  -8.57142857, -10.        , -11.42857143, -12.85714286]
    ])

    if check_array(
            ex_name, softmax.run_gradient_descent_iteration,
            exp_res, X, Y, zeros, alpha, lambda_factor, temp):
        return
    softmax.run_gradient_descent_iteration(X, Y, zeros, alpha, lambda_factor, temp)
    log(green("PASS"), ex_name, "")

def check_update_y():
    ex_name = "Update y"
    train_y = np.arange(0, 10)
    test_y = np.arange(9, -1, -1)
    exp_res = (
            np.array([0, 1, 2, 0, 1, 2, 0, 1, 2, 0]),
            np.array([0, 2, 1, 0, 2, 1, 0, 2, 1, 0])
            )
    if check_tuple(
            ex_name, softmax.update_y,
            exp_res, train_y, test_y):
        return
    log(green("PASS"), ex_name, "")

###Correction note:  check_project_onto_PC fucntion have been modified since release.
def check_project_onto_PC():
    ex_name = "Project onto PC"
    X = np.array([
        [1, 2, 3],
        [2, 4, 6],
        [3, 6, 9],
        [4, 8, 12],
    ]);
    x_centered, feature_means = features.center_data(X)
    pcs = features.principal_components(x_centered)
    exp_res = np.array([
        [5.61248608, 0],
        [1.87082869, 0],
        [-1.87082869, 0],
        [-5.61248608, 0],
    ])
    n_components = 2
    if check_array(
            ex_name, features.project_onto_PC,
            exp_res, X, pcs, n_components, feature_means):
        return
    log(green("PASS"), ex_name, "")

def check_polynomial_kernel():
    ex_name = "Polynomial kernel"
    n, m, d = 3, 5, 7
    c = 1
    p = 2
    X = np.random.random((n, d))
    Y = np.random.random((m, d))
    try:
        K = kernel.polynomial_kernel(X, Y, c, d)
    except NotImplementedError:
        log(red("FAIL"), ex_name, ": not implemented")
        return True
    for i in range(n):
        for j in range(m):
            exp = (X[i] @ Y[j] + c) ** d
            got = K[i][j]
            if (not equals(exp, got)):
                log(
                    red("FAIL"), ex_name,
                    ": values at ({}, {}) do not match. Expected {}, got {}"
                    .format(i, j, exp, got)
                )
    log(green("PASS"), ex_name, "")

def check_rbf_kernel():
    ex_name = "RBF kernel"
    n, m, d = 3, 5, 7
    gamma = 0.5
    X = np.random.random((n, d))
    Y = np.random.random((m, d))
    try:
        K = kernel.rbf_kernel(X, Y, gamma)
    except NotImplementedError:
        log(red("FAIL"), ex_name, ": not implemented")
        return True
    for i in range(n):
        for j in range(m):
            exp = np.exp(-gamma * (np.linalg.norm(X[i] - Y[j]) ** 2))
            got = K[i,j]
            #print(exp,got)
            if (not equals(exp, got)):
                log(
                    red("FAIL"), ex_name,
                    ": values at ({}, {}) do not match. Expected {}, got {}"
                    .format(i, j, exp, got)
                )
    log(green("PASS"), ex_name, "")


def main():
    log(green("PASS"), "Import mnist project")
    try:
        check_get_mnist()
        check_closed_form()
        check_svm()
        check_compute_probabilities()
        check_compute_cost_function()
        check_run_gradient_descent_iteration()
        check_update_y()
        check_project_onto_PC()
        check_polynomial_kernel()
        check_rbf_kernel()
    except Exception:
        log_exit(traceback.format_exc())

if __name__ == "__main__":
    main()
