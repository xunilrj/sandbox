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
        return np.isclose(x, y, atol=0.0001).all()
    return np.isclose(x, y)

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


    # TEST 1
    # perceptron_single_step_update 
    # input:
    feature_vector = np.array([0.06298069, 0.12247194,  0.43157583, -0.00275753, 0.21242322,  0.37834088, -0.00336766,  0.36649118, -0.05804748, -0.07044078])
    label = 1
    theta = np.array([ 0.27572318,  0.36217527, -0.03821352, -0.15492811,  0.15099115, -0.39773863, -0.43742569,  0.35090986,  0.18979834, -0.33198223])
    theta_0 = 0.1334537761399014
    exp_res = (np.array([0.2757232, 0.3621753, -0.0382135, -0.1549281, 0.1509911, -0.3977386, -0.4374257, 0.3509099, 0.1897983, -0.3319822]), 0.1334538)
    if check_tuple(
            ex_name, p1.perceptron_single_step_update,
            exp_res, feature_vector, label, theta, theta_0):
        return

    # TEST 2
    # perceptron_single_step_update input:
    feature_vector = np.array([-0.27358021, 0.19094896, 0.20727797, 0.47472374, -0.04748706, -0.17446884, 0.19482128, 0.47550921, -0.19288731, 0.11184685])
    label = 1
    theta = np.array([-0.05356288, 0.21418464,  0.29032238,  0.43994847, -0.33177068,  0.0193394, -0.14320006, -0.25780186,  0.40782174, 0.43216883])
    theta_0 = 0.012118259663534386
    exp_res = (np.array([-0.0535629, 0.2141846, 0.2903224, 0.4399485, -0.3317707, 0.0193394, -0.1432001, -0.2578019, 0.4078217, 0.4321688]), 0.0121183)
    if check_tuple(
            ex_name, p1.perceptron_single_step_update,
            exp_res, feature_vector, label, theta, theta_0):
        return

    # TEST 3
    feature_vector = np.array([-0.2028912,  -0.34173138, -0.0969048,   0.35590442, -0.49591787,  0.10763055, 0.35089003, -0.05640367, -0.06898151,  0.06224621])
    label = 1
    theta = np.array([-0.38528531,  0.042664,   -0.24688993 , 0.47107283,  0.49117305, -0.33306922, 0.35263492, -0.49168031, -0.26070439, -0.40917898])
    theta_0 = -0.2852097700194439
    exp_res = (np.array([-0.5881765, -0.2990674, -0.3437947, 0.8269772, -0.0047448, -0.2254387, 0.7035250, -0.5480840, -0.3296859, -0.3469328]), 0.7147902)
    if check_tuple(
            ex_name, p1.perceptron_single_step_update,
            exp_res, feature_vector, label, theta, theta_0):
        return

    # TEST 4
    #perceptron_single_step_update input:
    feature_vector = np.array([-0.38689505, -0.168472,   -0.20886875,  0.41268776,  0.4891808,   0.38699686, -0.48748489, -0.28093428,  0.3297704,  -0.32510744])
    label = 1
    theta = np.array([-0.15552279,  0.26432046, -0.06979896,  0.05080136,  0.29687601,  0.00337397, -0.09910963,  0.37477249,  0.41918328, -0.26912298])
    theta_0 = -0.3664722096017953
    exp_res = (np.array([-0.5424178, 0.0958485, -0.2786677, 0.4634891, 0.7860568, 0.3903708, -0.5865945, 0.0938382, 0.7489537, -0.5942304]), 0.6335278)
    if check_tuple(
            ex_name, p1.perceptron_single_step_update,
            exp_res, feature_vector, label, theta, theta_0):
        return

    # TEST 5
    feature_vector = np.array([0.19789242, -0.12419866, 0.39403371, 0.16481933, 0.42828959, 0.49353143, -0.3573384,  0.04309788, 0.49424988, -0.3578183])
    label = -1
    theta = np.array([0.36643212, 0.27751104, 0.15302509, -0.09095363, 0.19737292, 0.48563305, -0.35237083, -0.19985133, 0.3155349, 0.46672488])
    theta_0 = -0.5138145493839743
    exp_res = (np.array([0.1685397, 0.4017097, -0.2410086, -0.2557730, -0.2309167, -0.0078984, 0.0049676, -0.2429492, -0.1787150, 0.8245432]), -1.5138145)
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

    # TEST 1
    # feature_matrix = np.array([
    #     [0.20476929496970875, -0.1678145520530171, 0.48875388759275606, -0.10982100136689332, 0.1575684314811835, 0.25779619855819036, 0.38715901581363066, 0.4629011770588777, -0.4229108420013834, 0.2169702159546002],
    #     [-0.2495033930968874, -0.45772724808592335, 0.27861655879508307, 0.29285774384987473, -0.49892714283422546, 0.27664544646595346, -0.23925058966738033, -0.36178617964148096, 0.21078541803144546, -0.10734742373774231],
    #     [-0.4339492389747138, 0.31865986244685807, 0.01893765985052187, -0.42271045515267425, 0.3542154133971446, -0.126258079755029, -0.041548759388334466, 0.49504704345807515, 0.4343798500284063, 0.2143718704182619],
    #     [-0.20341752120265089, 0.15002073246542036, 0.3597394559271697, -0.2655785007250535, -0.10214784546037048, 0.3891079624916888, 0.4120600439615286, 0.07116932583963176, -0.29493674767227185, -0.25103464890329064],
    #     [-0.2231769464765202, -0.20061997561285816, 0.06651911522120357, -0.35606926647820214, -0.2585522042820606, 0.29095077383578405, 0.4886468713016848, -0.022489640246533993, 0.15970780590658118, -0.05739074934943422]
    # ])
    # labels = np.array( [-1, 1, -1, -1, 1] )
    # T = 5
    # exp_res = (np.array([0.1646864, -1.1270278, -0.0335414, 0.6250774, -1.0095469, 0.3047463, -0.1211150, -0.9504922, 0.2310501, -0.1280754]), 0)
    # if check_tuple(
    #         ex_name, p1.perceptron,
    #         exp_res, feature_matrix, labels, T):
    #     return

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


    # pegasos input:
    # feature_matrix = np.array([
    #     [ 0.1837462,   0.29989789, -0.35889786, -0.30780561, -0.44230703, -0.03043835, 0.21370063,  0.33344998, -0.40850817, -0.13105809],
    #     [ 0.08254096,  0.06012654,  0.19821234,  0.40958367,  0.07155838, -0.49830717, 0.09098162,  0.19062183, -0.27312663,  0.39060785],
    #     [-0.20112519, -0.00593087,  0.05738862,  0.16811148, -0.10466314, -0.21348009, 0.45806193, -0.27659307,  0.2901038,  -0.29736505],
    #     [-0.14703536, -0.45573697, -0.47563745, -0.08546162, -0.08562345,  0.07636098, -0.42087389, -0.16322197, -0.02759763,  0.0297091],
    #     [-0.18082261,  0.28644149, -0.47549449, -0.3049562,   0.13967768,  0.34904474, 0.20627692,  0.28407868,  0.21849356, -0.01642202],
    # ])
    # labels = np.array([-1, -1, -1,  1, -1])
    # T = 10
    # L = 0.1456692551041303
    # exp_res = (np.array([-0.0850387, -0.7286435, -0.3440130, -0.0560494, -0.0260993, 0.1446894, -0.8172203, -0.3200453, -0.0729161, 0.1008662]), 0)
    # if check_tuple(
    #         ex_name, p1.pegasos,
    #         exp_res, feature_matrix, labels, T, L):
    #     return

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
