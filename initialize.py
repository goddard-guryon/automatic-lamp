"""
Initialize positions and velocities
"""
from random import uniform, choice
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


def particle_shower(pos, vel, r, e_p, orig_n):
    """
    Add particles with force into the system
    """
    # how many particles we can add also depends on current
    # particle density in the entry strip
    # edit: as brilliant as this idea is (hehe), it hits right
    # at the bottleneck of the algorithm: adding even 2 particles
    # every x timesteps increases particle count very quickly,
    # and thus increases computation time x! times. So imma skip it
    # num = int((1-len([x for x in pos if 1-3*r<(x[1]-int(x[1]))<1-r])*2*r/len(pos))*len(pos)/20)

    # particles will have only variance in the x-coordinate
    new_pos, new_vel = [], []
    prob = uniform(0, 1) < (orig_n/len(pos) - .5)*2
    to_push = [b for a, b in zip(pos, vel) if int(a[1]) == e_p]
    if not to_push:
        prob = 1
    if prob:
        while True:
            new_pos = [uniform(r*1.05, 1-r*1.05), uniform(1-2*r*1.05, 1-r*1.05)+e_p]
            dist = min(sqrt((new_pos[0]-b[0])**2+(new_pos[1]-b[1])**2) for b in pos)
            if dist > 2*r:
                break

        # similarly, velocities will have almost no
        # velocity in the x-direction, and only go downwards
        new_vel = [[uniform(-.1, .1), -abs(normal(size=1)[0])]]
    else:

        # we don't always add a particle; sometimes we simply push
        # an existing particle
        v = choice(to_push)
        vel[vel.index(v)][1] -= abs(uniform(0, 1))
    if new_pos:
        return pos+[new_pos], vel+new_vel
    return pos, vel
