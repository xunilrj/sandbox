#!/usr/bin/python3
import numpy as np
import matplotlib.pyplot as plt

def solver(I, w, dt, T):  
    dt = float(dt)
    Nt = int(round(T/dt))
    u = np.zeros(Nt+1)
    t = np.linspace(0, Nt*dt, Nt+1)
    u[0] = I
    u[1] = u[0] - 0.5*dt**2*w**2*u[0]
    for n in range(1, Nt):
        u[n+1] = 2*u[n] - u[n-1] - dt**2*w**2*u[n]
    return u, t

def u_exact(t, I, w):
    return I*np.cos(w*t)

def visualize(u, t, I, w, dt):
    plt.plot(t, u, 'r--o')
    t_fine = np.linspace(0, t[-1], 1001) # very fine mesh for u_e
    u_e = u_exact(t_fine, I, w)
    plt.hold('on')
    plt.plot(t_fine, u_e, 'b-')
    plt.legend(['numerical', 'exact'], loc='upper left')
    plt.xlabel('t')
    plt.ylabel('u')
    dt = t[1] - t[0]
    plt.title('dt=%g' % dt)
    umin = 1.2*u.min()
    umax = -umin
    plt.axis([t[0], t[-1], umin, umax])
    plt.savefig('tmp1.png')
    plt.savefig('tmp1.pdf')