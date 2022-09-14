"""
EdMD simulation
"""
import os
from math import sqrt
from random import uniform
from maze import make_maze
import matplotlib.pyplot as plt
import cv2


def get_sg_pr(n):
    """
    Create wall and pair permutations of disks
    """
    walls = [(a, b) for a in range(n) for b in range(2)]
    pairs = [(a, b) for a in range(n) for b in range(a+1, n)]
    return walls, pairs


def wall_time(pos, v, r, k, l, edges):
    """
    Time before a particle hits a wall
    """
    x = pos[k][l]
    i, j = [int(a) for a in pos[k]]
    if not l:
        if v > 0:
            loc = [((i+1, j), (i+1, j+1)), ((i+1, j+1), (i+1, j))]
        else:
            loc = [((i, j), (i, j+1)), ((i, j+1), (i, j))]
    else:
        if v > 0:
            loc = [((i, j+1), (i+1, j+1)), ((i+1, j+1), (i, j+1))]
        else:
            loc = [((i, j), (i+1, j)), ((i+1, j), (i, j))]
    if any(a in edges for a in loc):
        if v > 0:
            return abs(-(x%(-1))-r)/v
            if val > 0:
                return val
            return abs(val)
        if v < 0:
            val = ((x%1)-r)/abs(v)
            if val > 0:
                return val
            return abs(val)
    return float("inf")


def pair_time(x_a, v_a, x_b, v_b, r):
    """
    Time before two particles hit each other
    """
    (dx, dx_2), (dv, dv_2) = map(lambda a, b: ([b[0]-a[0], b[1]-a[1]],
                                               (b[0]-a[0])**2 + (b[1]-a[1])**2),
                                 (x_a, v_a), (x_b, v_b))
    b_ij = dv[0]*dx[0] + dv[1]*dx[1]
    upsilon = b_ij**2 - dv_2*(dx_2 - 4*r**2)
    return -(b_ij + sqrt(upsilon))/dv_2 if b_ij < 0 < upsilon else float("inf")


def get_next_event(p, v, singles, pairs, s, edges):
    """
    Find which event happens next
    """
    walls = [wall_time(p, v[k][l], s, k, l, edges) for k, l in singles]
    pairs = [pair_time(p[k], v[k], p[l], v[l], s) for k, l in pairs]
    return walls+pairs, min(zip(walls+pairs, range(len(walls+pairs))))


def get_velocities(pos, vel, singles, pairs, n_ix, edges):
    """
    Change velocities after event
    """
    if n_ix < len(singles):
        disk, direction = singles[n_ix]
        vel[disk][direction] *= -1
    else:
        a, b = pairs[n_ix - len(singles)]
        dx = [pos[b][0]-pos[a][0], pos[b][1]-pos[a][1]]
        x_ij = sqrt(dx[0]**2 + dx[1]**2)
        x_ij = [q/x_ij for q in dx]

        dv = [vel[b][0]-vel[a][0], vel[b][1]-vel[a][1]]
        b_ij = dv[0]*x_ij[0] + dv[1]*x_ij[1]
        for k in range(2):
            vel[a][k] += x_ij[k]*b_ij
            vel[b][k] -= x_ij[k]*b_ij
    return vel


def fix_delta(pos, vel, r, edges, l):
    """
    Basically bug fixes:
    - there may be disks that didn't collide with wall,
      and are now overlapping with it; bounce em back
    - disks tend to stick to walls if they hit at certain
      angles; unstick them
    """
    x, y = pos
    i, j = [int(a) for a in pos]
    if not l:
        if x < i+1 < x+r and vel[l] > 0:
            loc = [((i+1, j), (i+1, j+1)), ((i+1, j+1), (i+1, j))]
            if any(a in edges for a in loc):
                vel[l] *= -1
        elif x-r < i < x and vel[l] < 0:
            loc = [((i, j), (i, j+1)), ((i, j+1),(i, j))]
            if any(a in edges for a in loc):
                vel[l] *= -1
    else:
        if y < j+1 < y+r and vel[l] > 0:
            loc = [((i, j+1), (i+1, j+1)), ((i+1, j+1), (i, j+1))]
            if any(a in edges for a in loc):
                vel[l] *= -1
        elif y-r < j < y and vel[l] < 0:
            loc = [((i, j), (i+1, j)), ((i+1, j), (i, j))]
            if any(a in edges for a in loc):
                vel[l] *= -1
    return vel[l]


def save_img(i, p, v, r, edges, output_dir, with_arrows, arrow_scale=.2, ex=None, ey=None):
    """
    Save a snapshot of simulation
    """
    if not os.path.exists(output_dir):
        os.mkdir(output_dir)
    plt.clf()
    plt.xticks([])
    plt.yticks([])
    for e in edges:
        plt.plot([e[0][0], e[1][0]], [e[0][1], e[1][1]], "b-")
    for (x, y), (dx, dy) in zip(p, v):
        dx *= arrow_scale
        dy *= arrow_scale
        col = 'b'
        if ex and ey:
            if ex-1 < x < ex and y+r < ey:
                col = 'r'
        circle = plt.Circle((x, y), radius=r, fc=col)
        plt.gca().add_patch(circle)
        if with_arrows:
            plt.arrow(x, y, dx, dy, fc='k', ec='k', head_length=.05, head_width=.05)
    plt.savefig(os.path.join(output_dir, f"{i}.png"))
    return 0


def run_simulation(n, p, v, r, edges, n_events, dt=5e-5, with_arrows=False, out="simulation_snapshots"):
    """
    Run Molecular Dynamics simulation
    """
    # find the exit point
    max_x, max_y = max(x[1][0] for x in edges), max(x[1][1] for x in edges)

    sg, pr = get_sg_pr(n)

    if os.path.isdir(out):
        print("Output directory already exists, deleting any files within it...")
        for file in os.listdir(out):
            os.remove(f"{out}/{file}")
    t, i = 0, 0

    w, (next_event, next_event_ix) = get_next_event(p, v, sg, pr, r, edges)
    save_img(i, p, v, r, edges, out, with_arrows)
    i += 1
    for _ in range(n_events):
        if dt:
            next_t = t + dt
        else:
            next_t = t + next_event

        q = 0
        v_old = v
        while t+next_event <= next_t:
            print(f"\033[KSimulating timestep {t:.5f} s; run {q}\r", end='', flush=True)
            if q > 100 and all(abs(v[k][l]) == abs(v_old[k][l]) for k, l in sg):

                # clearly, we're just stuck here, doing nothing,
                # so just skip to next event
                step = max(dt, next_event)
            else:

                # but if not, iterate over the smallest timestep
                step = min(dt, next_event)
            q += 1
            t += step
            for k, l in sg:

                # for debugging
                v[k][l] = fix_delta(p[k], v[k], r, edges, l)
                p[k][l] += v[k][l]*step
            with open("simulation.log", 'a') as file:
                file.write(f"time: {t}, i: {i}\nPositions:\n")
                for pos in p:
                    file.write(''.join(str(pos)) + '\n')
                file.write('Velocities\n')
                for vel in v:
                    file.write(''.join(str(vel))+ '\n')
            v = get_velocities(p, v, sg, pr, next_event_ix, edges)
            w, (next_event, next_event_ix) = get_next_event(p, v, sg, pr, r, edges)
            if next_event < 0:
                with open("simulation.log", 'a') as file:
                    file.write(f"{t}, {i}, {next_event}")
                    for line in w:
                        file.write(str(line) + ' ')
                return 1
            v_old = v
        remain_t = next_t - t
        for k, l in sg:
            v[k][l] = fix_delta(p[k], v[k], r, edges, l)
            p[k][l] += v[k][l]*remain_t
        t += remain_t
        next_event -= remain_t

        if not i%2000:
            save_img(i//2000, p, v, r, edges, out, with_arrows)
        i += 1
        print(f"\033[KSimulating timestep {t:.5f} s\r", end='', flush=True)

        # check if any particle has reached the exit
        for pos in p:
            if max_x-1 < pos[0] < max_x and pos[1]+r < 0:
                save_img("final", p, v, r, edges, out, with_arrows, max_x, 0)
                print(f"\nTimestep {t:.5f}; a particle solved the maze! Halting")
                return 0
    # convert all snapshots to video
    #print("\nConverting simulation data to video")
    #imgs = sorted([f for f in os.listdir(out) if f.endswith(".png")],
    #              key=lambda x: int(x.split('.')[0]))
    #height, width, _ = cv2.imread(f"{out}/{imgs[0]}").shape
    #video = cv2.VideoWriter(f"{out}/animation.avi", 0, 20, (width, height))
    #for img in imgs:
    #    video.write(cv2.imread(f"{out}/{img}"))
    #video.release()
    print("\nFinished simulation")
    return 0
