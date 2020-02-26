import os
import sys
import time
import traceback
import project1 as p1
import numpy as np

verbose = False

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
    if res != exp_res:
        log(red("FAIL"), ex_name, ": incorrect answer. Expected", exp_res, ", got: ", res)
        return True


def equals(x, y):
    if type(y) == np.ndarray:
        return (x == y).all()
    return x == y

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
    if not all(equals(x, y) for x, y in zip(res, exp_res)):
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


def check_get_order():
    ex_name = "Get order"
    if check_list(
            ex_name, p1.get_order,
            [0], 1):
        log("You should revert `get_order` to its original implementation for this test to pass")
        return
    if check_list(
            ex_name, p1.get_order,
            [1, 0], 2):
        log("You should revert `get_order` to its original implementation for this test to pass")
        return
    log(green("PASS"), ex_name, "")


def check_hinge_loss_single():
    ex_name = "Hinge loss single"

    feature_vector = np.array([1, 2])
    label, theta, theta_0 = 1, np.array([-1, 1]), -0.2
    exp_res = 1 - 0.8
    if check_real(
            ex_name, p1.hinge_loss_single,
            exp_res, feature_vector, label, theta, theta_0):
        return
    log(green("PASS"), ex_name, "")


def check_hinge_loss_full():
    ex_name = "Hinge loss full"

    #feature_vector = np.array([[1, 2], [1, 2]])
    #label, theta, theta_0 = np.array([1, 1]), np.array([-1, 1]), -0.2
    #exp_res = 1 - 0.8

    # TEST 1
    # feature_vector = np.array([
    #     [0.03766714, 0.34852438, 0.10416706, 0.892713, 0.36784852, 0.16037263, 0.79133375, 0.06844007, 0.41992702, 0.61444322],
    #     [0.92505734, 0.52660135, 0.50986554, 0.31291756, 0.43919298, 0.97769081, 0.92608606, 0.38263444, 0.08789442, 0.75065879],
    #     [0.53053297, 0.63978153, 0.59123774, 0.11952923, 0.46410245, 0.68554495, 0.49069175, 0.17861742, 0.14980658, 0.235896  ],
    #     [0.72326644, 0.61139761, 0.07469191, 0.52737737, 0.7295717,  0.97550641, 0.23231529, 0.33765573, 0.52297792, 0.12435942],
    #     [0.31544772, 0.92318883, 0.3539478,  0.22489902, 0.83124767, 0.57552165, 0.50583014, 0.19226411, 0.59415012, 0.19137411],
    #     [0.78135387, 0.6486907,  0.33282546, 0.33151597, 0.87558229, 0.91762086, 0.68878532, 0.18222975, 0.18758408, 0.52951619],
    #     [0.8640821 , 0.51960039, 0.3193202,  0.10336577, 0.71774689, 0.33072073, 0.90534314, 0.72823252, 0.58351268, 0.5281091 ],
    #     [0.47893208, 0.75647929, 0.27207122, 0.35983166, 0.54968602, 0.78383511, 0.40147155, 0.19885448, 0.34377753, 0.19128855],
    #     [0.54843492, 0.36919709, 0.51752134, 0.7238138,  0.83963101, 0.73045304, 0.0516664,  0.79620159, 0.93671986, 0.57588284],
    #     [0.65119138, 0.96397813, 0.1768498,  0.52863491, 0.61677868, 0.38482147, 0.54141091, 0.4340331,  0.46373625, 0.43400199]])
    # label = np.array([1, 0, 2, 1, 2, 0, 2, 2, 1, 0])
    # theta = np.array([0.37977733, 0.33056082, 0.18380417, 0.6847391, 0.74845345, 0.67317365, 0.43955155, 0.44113653, 0.82783179, 0.53855043])
    # theta_0 = 0    
    # exp_res = 0.3

    # TEST 2
    # feature_vector = np.array([
    #     [1., 1., 1., 1.],
    #     [1., 1., 1., 1.],
    #     [1., 1., 1., 1.]])
    # label = [1., 1., 1.]
    # theta = [1., 1., 1., 1.]
    # theta_0 = -3
    # exp_res = 0

    # TEST 3
    # feature_vector = np.array([
    #     [ 3.70752282,  0.11251751,  3.63609098, -1.82673257,  3.46592548],
    #     [ 1.34824939, -0.34295169,  1.19526055,  0.07293385, -0.53298844],
    #     [ 2.95954691, -0.27806892,  3.27449194, -0.73060663, -0.95582739],
    #     [ 1.31340351, -1.54873403,  1.20388711, -1.40314974,  1.11317539],
    #     [ 3.13463404, -0.94393889, -1.82457749, -1.87405909, -1.03870385],
    #     [-0.4079261 ,  0.44207802,  2.25305111,  0.51183395,  2.90853109],
    #     [-1.24735041, -0.33819895,  1.13234472,  2.57892447, -0.00610873]])
    # label = [ 1.,  1., -1., -1., -1.,  1., -1.]
    # theta = [-1.89448558, -0.17152266, -0.40119769, 1.64531582, -1.35101277]
    # theta_0 = -0.32
    # exp_res = 4.639621101290755

    #test 4
    feature_vector = np.array([
        [-0.18472826, -3.27551449,  3.9109146,   4.0114707,  -4.37379166],
        [-1.37956983,  1.91327236,  4.80435275,  6.09625954, -2.07265713],
        [ 3.04198902, -3.07178737, -1.52057115,  3.3385423,   0.54610877],
        [ 5.39100329, -0.49406049, -2.52078092, -0.45947861,  6.70556531],
        [ 4.05378793,  5.93132543,  3.23167942, -4.38850912,  6.75472453],
        [ 6.9162901 , -1.85141089, -1.69416199,  4.55784365, -3.55049711],
        [-3.86241827,  4.62693769,  6.19761984,  5.06163544, -1.11978026],
        [ 4.55913232,  6.30362193,  3.7535506,   6.72269519,  2.22327723],
        [-1.04733321, -2.33002512,  2.3200751,  -3.321577 ,   4.17192468],
        [ 2.995456  , -3.68137797, -3.6864026,  -2.85922505,  1.4804962 ],
        [ 6.42084371, -1.68343244, -1.09284653, -1.64111652, -4.74029965],
        [ 2.94280857, -0.77494597,  2.82745767, -2.45817565, -0.75311432],
        [-3.35238494,  3.84833403, -0.92923066,  6.96256688,  1.14271564],
        [-0.69151667,  2.65114568,  0.80228413, -0.70576065, -3.77167494],
        [-2.61930758,  5.59319241,  2.5834452,   6.43903521,  4.61741086],
        [ 3.77957128, -2.06180702, -0.20458118, -0.906144 ,  -1.2175165 ],
        [ 1.25280172,  4.55291367,  5.2023342,  -1.28921724,  1.87728942]])
    label = [ 1., -1., -1.,  1., -1.,  1., -1.,  1.,  1., -1.,  1., -1., -1.,  1.,  1., -1.,  1.]
    theta = [-1.47729689,  0.80575773,  1.36302739,  0.56973226, -0.24280451]
    theta_0 = -1
    exp_res = 5.29731721188455
    if check_real(
            ex_name, p1.hinge_loss_full,
            exp_res, feature_vector, label, theta, theta_0):
        return

    log(green("PASS"), ex_name, "")


def check_perceptron_single_update():
    ex_name = "Perceptron single update"

    feature_vector = np.array([1, 2])
    label, theta, theta_0 = 1, np.array([-1, 1]), -1.5
    exp_res = (np.array([0, 3]), -0.5)
    if check_tuple(
            ex_name, p1.perceptron_single_step_update,
            exp_res, feature_vector, label, theta, theta_0):
        return

    feature_vector = np.array([1, 2])
    label, theta, theta_0 = 1, np.array([-1, 1]), -1
    exp_res = (np.array([0, 3]), 0)
    if check_tuple(
            ex_name + " (boundary case)", p1.perceptron_single_step_update,
            exp_res, feature_vector, label, theta, theta_0):
        return

    log(green("PASS"), ex_name, "")


def check_perceptron():
    ex_name = "Perceptron"

    feature_matrix = np.array([[1, 2]])
    labels = np.array([1])
    T = 1
    exp_res = (np.array([1, 2]), 1)
    if check_tuple(
            ex_name, p1.perceptron,
            exp_res, feature_matrix, labels, T):
        return

    feature_matrix = np.array([[1, 2], [-1, 0]])
    labels = np.array([1, 1])
    T = 1
    exp_res = (np.array([0, 2]), 2)
    if check_tuple(
            ex_name, p1.perceptron,
            exp_res, feature_matrix, labels, T):
        return

    feature_matrix = np.array([[1, 2]])
    labels = np.array([1])
    T = 2
    exp_res = (np.array([1, 2]), 1)
    if check_tuple(
            ex_name, p1.perceptron,
            exp_res, feature_matrix, labels, T):
        return

    feature_matrix = np.array([[1, 2], [-1, 0]])
    labels = np.array([1, 1])
    T = 2
    exp_res = (np.array([0, 2]), 2)
    if check_tuple(
            ex_name, p1.perceptron,
            exp_res, feature_matrix, labels, T):
        return

    log(green("PASS"), ex_name, "")


def check_average_perceptron():
    ex_name = "Average perceptron"

    feature_matrix = np.array([[1, 2]])
    labels = np.array([1])
    T = 1
    exp_res = (np.array([1, 2]), 1)
    if check_tuple(
            ex_name, p1.average_perceptron,
            exp_res, feature_matrix, labels, T):
        return

    feature_matrix = np.array([[1, 2], [-1, 0]])
    labels = np.array([1, 1])
    T = 1
    exp_res = (np.array([-0.5, 1]), 1.5)
    if check_tuple(
            ex_name, p1.average_perceptron,
            exp_res, feature_matrix, labels, T):
        return

    feature_matrix = np.array([[1, 2]])
    labels = np.array([1])
    T = 2
    exp_res = (np.array([1, 2]), 1)
    if check_tuple(
            ex_name, p1.average_perceptron,
            exp_res, feature_matrix, labels, T):
        return

    feature_matrix = np.array([[1, 2], [-1, 0]])
    labels = np.array([1, 1])
    T = 2
    exp_res = (np.array([-0.25, 1.5]), 1.75)
    if check_tuple(
            ex_name, p1.average_perceptron,
            exp_res, feature_matrix, labels, T):
        return

    log(green("PASS"), ex_name, "")


def check_pegasos_single_update():
    ex_name = "Pegasos single update"

    feature_vector = np.array([1, 2])
    label, theta, theta_0 = 1, np.array([-1, 1]), -1.5
    L = 0.2
    eta = 0.1
    exp_res = (np.array([-0.88, 1.18]), -1.4)
    if check_tuple(
            ex_name, p1.pegasos_single_step_update,
            exp_res,
            feature_vector, label, L, eta, theta, theta_0):
        return

    feature_vector = np.array([1, 1])
    label, theta, theta_0 = 1, np.array([-1, 1]), 1
    L = 0.2
    eta = 0.1
    exp_res = (np.array([-0.88, 1.08]), 1.1)
    if check_tuple(
            ex_name +  " (boundary case)", p1.pegasos_single_step_update,
            exp_res,
            feature_vector, label, L, eta, theta, theta_0):
        return

    feature_vector = np.array([1, 2])
    label, theta, theta_0 = 1, np.array([-1, 1]), -2
    L = 0.2
    eta = 0.1
    exp_res = (np.array([-0.88, 1.18]), -1.9)
    if check_tuple(
            ex_name, p1.pegasos_single_step_update,
            exp_res,
            feature_vector, label, L, eta, theta, theta_0):
        return

    log(green("PASS"), ex_name, "")


def check_pegasos():
    ex_name = "Pegasos"

    feature_matrix = np.array([[1, 2]])
    labels = np.array([1])
    T = 1
    L = 0.2
    exp_res = (np.array([1, 2]), 1)
    if check_tuple(
            ex_name, p1.pegasos,
            exp_res, feature_matrix, labels, T, L):
        return

    feature_matrix = np.array([[1, 1], [1, 1]])
    labels = np.array([1, 1])
    T = 1
    L = 1
    exp_res = (np.array([1-1/np.sqrt(2), 1-1/np.sqrt(2)]), 1)
    if check_tuple(
            ex_name, p1.pegasos,
            exp_res, feature_matrix, labels, T, L):
        return

    log(green("PASS"), ex_name, "")


def check_classify():
    ex_name = "Classify"

    feature_matrix = np.array([[1, 1], [1, 1], [1, 1]])
    theta = np.array([1, 1])
    theta_0 = 0
    exp_res = np.array([1, 1, 1])
    if check_array(
            ex_name, p1.classify,
            exp_res, feature_matrix, theta, theta_0):
        return

    feature_matrix = np.array([[-1, 1]])
    theta = np.array([1, 1])
    theta_0 = 0
    exp_res = np.array([-1])
    if check_array(
            ex_name + " (boundary case)", p1.classify,
            exp_res, feature_matrix, theta, theta_0):
        return

    log(green("PASS"), ex_name, "")

def check_classifier_accuracy():
    ex_name = "Classifier accuracy"

    train_feature_matrix = np.array([[1, 0], [1, -1], [2, 3]])
    val_feature_matrix = np.array([[1, 1], [2, -1]])
    train_labels = np.array([1, -1, 1])
    val_labels = np.array([-1, 1])
    exp_res = 1, 0
    T=1
    if check_tuple(
            ex_name, p1.classifier_accuracy,
            exp_res,
            p1.perceptron,
            train_feature_matrix, val_feature_matrix,
            train_labels, val_labels,
            T=T):
        return

    train_feature_matrix = np.array([[1, 0], [1, -1], [2, 3]])
    val_feature_matrix = np.array([[1, 1], [2, -1]])
    train_labels = np.array([1, -1, 1])
    val_labels = np.array([-1, 1])
    exp_res = 1, 0
    T=1
    L=0.2
    if check_tuple(
            ex_name, p1.classifier_accuracy,
            exp_res,
            p1.pegasos,
            train_feature_matrix, val_feature_matrix,
            train_labels, val_labels,
            T=T, L=L):
        return

    log(green("PASS"), ex_name, "")

def check_bag_of_words():
    ex_name = "Bag of words"

    texts = [
        "He loves to walk on the beach",
        "There is nothing better"]

    try:
        res = p1.bag_of_words(texts)
    except NotImplementedError:
        log(red("FAIL"), ex_name, ": not implemented")
        return
    if not type(res) == dict:
        log(red("FAIL"), ex_name, ": does not return a tuple, type: ", type(res))
        return

    vals = sorted(res.values())
    exp_vals = list(range(len(res.keys())))
    if not vals == exp_vals:
        log(red("FAIL"), ex_name, ": wrong set of indices. Expected: ", exp_vals, " got ", vals)
        return

    log(green("PASS"), ex_name, "")

    keys = sorted(res.keys())
    exp_keys = ['beach', 'better', 'he', 'is', 'loves', 'nothing', 'on', 'the', 'there', 'to', 'walk']
    stop_keys = ['beach', 'better', 'loves', 'nothing', 'walk']

    if keys == exp_keys:
        log(yellow("WARN"), ex_name, ": does not remove stopwords:", [k for k in keys if k not in stop_keys])
    elif keys == stop_keys:
        log(green("PASS"), ex_name, " stopwords removed")
    else:
        log(red("FAIL"), ex_name, ": keys are missing:", [k for k in stop_keys if k not in keys], " or are not unexpected:", [k for k in keys if k not in stop_keys])


def check_extract_bow_feature_vectors():
    ex_name = "Extract bow feature vectors"
    texts = [
        "He loves her ",
        "He really really loves her"]
    keys = ["he", "loves", "her", "really"]
    dictionary = {k:i for i, k in enumerate(keys)}
    exp_res = np.array(
        [[1, 1, 1, 0],
        [1, 1, 1, 1]])
    non_bin_res = np.array(
        [[1, 1, 1, 0],
        [1, 1, 1, 2]])


    try:
        res = p1.extract_bow_feature_vectors(texts, dictionary)
    except NotImplementedError:
        log(red("FAIL"), ex_name, ": not implemented")
        return

    if not type(res) == np.ndarray:
        log(red("FAIL"), ex_name, ": does not return a numpy array, type: ", type(res))
        return
    if not len(res) == len(exp_res):
        log(red("FAIL"), ex_name, ": expected an array of shape ", exp_res.shape, " but got array of shape", res.shape)
        return

    log(green("PASS"), ex_name)

    if (res == exp_res).all():
        log(yellow("WARN"), ex_name, ": uses binary indicators as features")
    elif (res == non_bin_res).all():
        log(green("PASS"), ex_name, ": correct non binary features")
    else:
        log(red("FAIL"), ex_name, ": unexpected feature matrix")
        return

def main():
    log(green("PASS"), "Import project1")
    try:
        check_get_order()
        check_hinge_loss_single()
        check_hinge_loss_full()
        check_perceptron_single_update()
        check_perceptron()
        check_average_perceptron()
        check_pegasos_single_update()
        check_pegasos()
        check_classify()
        check_classifier_accuracy()
        check_bag_of_words()
        check_extract_bow_feature_vectors()
    except Exception:
        log_exit(traceback.format_exc())

if __name__ == "__main__":
    main()
