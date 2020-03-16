import numpy as np

### Functions for you to fill in ###

# Case 1
# X: [[0.30005398 0.34411061 0.92137676 0.15236684]
#  [0.01440052 0.98192039 0.50978332 0.02422831]
#  [0.01024311 0.70671807 0.04970452 0.19915923]
#  [0.79894777 0.41686102 0.64688285 0.62203202]
#  [0.04759948 0.98135193 0.69633345 0.99904601]
#  [0.54159911 0.7816082  0.00174544 0.31694975]
#  [0.78388827 0.72333568 0.82999496 0.84902681]
#  [0.64055927 0.22612963 0.08920106 0.04759164]
#  [0.7829167  0.31177873 0.09130675 0.17134589]
#  [0.34171202 0.29215057 0.10814357 0.04321877]
#  [0.9831187  0.35583935 0.91588261 0.57063751]
#  [0.61200157 0.31837751 0.5311759  0.28538612]
#  [0.97681325 0.68166692 0.60441009 0.39943611]
#  [0.2963774  0.69284509 0.73955334 0.74242144]]
# Y: [0.65214556 0.87508007 0.81611914 0.2161141  0.96215767 0.42135572
#  0.75119847 0.39427326 0.38962543 0.12269482 0.24213418 0.23329518
#  0.96179959 0.26247474]
# lambda_factor: 0.6925442348296535
# Submission output: [ 0.0710368   0.67089485  0.1714135  -0.02660626]
def closed_form(X, Y, lambda_factor):
    """
    Computes the closed form solution of linear regression with L2 regularization

    Args:
        X - (n, d + 1) NumPy array (n datapoints each with d features plus the bias feature in the first dimension)
        Y - (n, ) NumPy array containing the labels (a number from 0-9) for each
            data point
        lambda_factor - the regularization constant (scalar)
    Returns:
        theta - (d + 1, ) NumPy array containing the weights of linear regression. Note that theta[0]
        represents the y-axis intercept of the model and therefore X[0] = 1
    """
    Xt = np.transpose(X)
    I = np.eye(X.shape[1]) 
    A = Xt @ X + (I * lambda_factor)
    return np.linalg.inv(A) @ Xt @ Y

### Functions which are already complete, for you to use ###

def compute_test_error_linear(test_x, Y, theta):
    test_y_predict = np.round(np.dot(test_x, theta))
    test_y_predict[test_y_predict < 0] = 0
    test_y_predict[test_y_predict > 9] = 9
    return 1 - np.mean(test_y_predict == Y)
