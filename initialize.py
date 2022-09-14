"""
Initialize positions and velocities
"""
from random import uniform
from math import sqrt
from numpy.random import normal


def initial_pos(n, r):
    """
    Initialize positions of disk in 1x1 box
    """
    # going for random sequential deposition instead of Monte Carlo
    # since we don't care about sampling the phase space 'appropriately'
    box = [[uniform(r, 1-r), uniform(r, 1-r)]]
    for _ in range(1, n):
        while True:
            new = [uniform(r, 1-r), uniform(r, 1-r)]
            dist = min(sqrt((new[0]-b[0])**2 + (new[1]-b[1])**2) for b in box)
            if dist > 2*r:
                break
        box.append(new)
        if len(box) == n:
            return box
    return 1


def initial_vel(n):
    """
    Initialize velocities of disks using
    Maxwell-Boltzmann distribution
    """
    # was using Gaussian distribution at first, but
    # then discovered this numpy function which is,
    # apparently, better suited for this purpose
    v_x, v_y = normal(size=n), normal(size=n)
    vel = [[v_x[i], v_y[i]] for i in range(n)]
    return vel
