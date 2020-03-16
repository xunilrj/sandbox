import numpy as np
from sklearn.svm import LinearSVC


### Functions for you to fill in ###

def one_vs_rest_svm(train_x, train_y, test_x):
    """
    Trains a linear SVM for binary classifciation

    Args:
        train_x - (n, d) NumPy array (n datapoints each with d features)
        train_y - (n, ) NumPy array containing the labels (0 or 1) for each training data point
        test_x - (m, d) NumPy array (m datapoints each with d features)
    Returns:
        pred_test_y - (m,) NumPy array containing the labels (0 or 1) for each test data point
    """
    # So c is essentially 1 over lambda.
    # So as C is large, we emphasize losses.
    # As c is small, we emphasize simpler solutions
    # that have a large margin.

    # For large values of C, the optimization will 
    # choose a smaller-margin hyperplane if that
    # hyperplane does a better job of getting all
    # the training points classified correctly.
    # https://stats.stackexchange.com/questions/31066/what-is-the-influence-of-c-in-svms-with-linear-kernel
    clf = LinearSVC(max_iter=1000, random_state=0, C=0.1)
    clf.fit(train_x, train_y)
    return clf.predict(test_x)

def multi_class_svm(train_x, train_y, test_x):
    """
    Trains a linear SVM for multiclass classifciation using a one-vs-rest strategy

    Args:
        train_x - (n, d) NumPy array (n datapoints each with d features)
        train_y - (n, ) NumPy array containing the labels (int) for each training data point
        test_x - (m, d) NumPy array (m datapoints each with d features)
    Returns:
        pred_test_y - (m,) NumPy array containing the labels (int) for each test data point
    """
    clf = LinearSVC(max_iter=1000, random_state=0, C=0.1, multi_class="ovr")
    clf.fit(train_x, train_y)
    return clf.predict(test_x)


def compute_test_error_svm(test_y, pred_test_y):
    return 1 - np.mean(pred_test_y == test_y)

