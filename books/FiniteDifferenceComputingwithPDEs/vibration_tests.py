from math import pi
import numpy as np
import matplotlib.pyplot as plt
from vibration_undamped import *

def test_three_steps():
    I = 1; w = 2*pi; dt = 0.1; T = 1
    u_by_hand = np.array([1.000000000000000, 0.802607911978213, 0.288358920740053])
    u, t = solver(I, w, dt, T)
    diff = np.abs(u_by_hand - u[:3]).max()
    tol = 1E-14
    assert diff < tol
