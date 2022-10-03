"""
EdMD simulation
"""
import os
from math import sqrt, cos, atan
from random import uniform
from initialize import initialize

particle_shower = initialize.particle_shower


def get_bx_pr(n: int) -> tuple:
    """
    Create wall and pair permutations of disks
    """
    walls = [(a, b) for a in range(n) for b in range(2)]
    pairs = [(a, b) for a in range(n) for b in range(a+1, n)]
    return walls, pairs


def _nearest_wall(i: int, j: int, dxn: int) -> list:
    """
    Get edge value for the nearest wall
    Backend function for fix_delta()
    """
    match dxn:
        case 1:  # right wall
            return [((i+1, j), (i+1, j+1)), ((i+1, j+1), (i+1, j))]
        case 2:  # left wall
            return [((i, j), (i, j+1)), ((i, j+1),(i, j))]
        case 3:  # top wall
            return [((i, j+1), (i+1, j+1)), ((i+1, j+1), (i, j+1))]
        case 4:  # bottom wall
            return [((i, j), (i+1, j)), ((i+1, j), (i, j))]


def save_log(logfile: str, t: float, i: int, pos: list, vel: list) -> bool:
    """
    Save simulation data in log file
    """
    with open(logfile, 'a') as file:
        file.write(f"time: {t} i: {i}\n")
        for p in pos:
            file.write(f"pos {p[0]} {p[1]}\n")
        for v in vel:
            file.write(f"vel {v[0]} {v[1]}\n")
    return 0


def wall_time(pos: list, vel: float, r: float, k: int, l: int, edges: list) -> float:
    """
    Time before a particle hits a wall
    """
    x, v = pos[k][l], vel[k][l]
    i, j = [int(a) for a in pos[k]]
    if not l:
        if v > 0:
            loc = _nearest_wall(i, j, 1)
        else:
            loc = _nearest_wall(i, j, 2)
    else:
        if v > 0:
            loc = _nearest_wall(i, j, 3)
        else:
            loc = _nearest_wall(i, j, 4)
    if any(a in edges for a in loc):
        if v > 0:
            return abs((-(x%(-1))-r)/v)
        if v < 0:
            return abs(((x%1)-r)/abs(v))
    return float("inf")


def pair_time(x_a: list, v_a: list, x_b: list, v_b: list, r: float) -> float:
    """
    Time before two particles hit each other
    """
    (dx, dx_2), (dv, dv_2) = map(lambda a, b: ([b[0]-a[0], b[1]-a[1]],
                                               (b[0]-a[0])**2 + (b[1]-a[1])**2),
                                 (x_a, v_a), (x_b, v_b))
    b_ij = dv[0]*dx[0] + dv[1]*dx[1]
    upsilon = b_ij**2 - dv_2*(dx_2 - 4*r**2)
    return -(b_ij + sqrt(upsilon))/dv_2 if b_ij < 0 < upsilon else float("inf")


def get_next_event(p: list, v: list, boxs: list, pairs: list, s: float, edges: list) -> tuple:
    """
    Find which event happens next
    """
    walls = [wall_time(p, v, s, k, l, edges) for k, l in boxs]
    pairs = [pair_time(p[k], v[k], p[l], v[l], s) for k, l in pairs]
    return min(zip(walls+pairs, range(len(walls+pairs))))


def get_velocities(pos: list, vel: list, boxs: list, pairs: list, n_ix: int) -> list:
    """
    Change velocities after event
    """
    if n_ix < len(boxs):
        disk, direction = boxs[n_ix]
        vel[disk][direction] *= -1
    else:
        a, b = pairs[n_ix - len(boxs)]
        dx = [pos[b][0]-pos[a][0], pos[b][1]-pos[a][1]]
        x_ij = sqrt(dx[0]**2 + dx[1]**2)
        x_ij = [q/x_ij for q in dx]

        dv = [vel[b][0]-vel[a][0], vel[b][1]-vel[a][1]]
        b_ij = dv[0]*x_ij[0] + dv[1]*x_ij[1]
        for k in range(2):
            vel[a][k] += x_ij[k]*b_ij
            vel[b][k] -= x_ij[k]*b_ij
    return vel


def fix_delta(pos: list, vel: list, r: float, edges: list, l: int) -> tuple:
    """
    Basically bug fix:
    - there may be disks that didn't collide with wall,
      and are now overlapping with it; bounce em back
    """
    x, y = pos
    i, j = [int(a) for a in pos]
    if not l:
        if x < i+1 < x+r and vel[l] > 0:
            loc = _nearest_wall(i, j, 1)
            if any(a in edges for a in loc):
                vel[l] *= -1
        elif x-r < i < x and vel[l] < 0:
            loc = _nearest_wall(i, j, 2)
            if any(a in edges for a in loc):
                vel[l] *= -1
        if y < j+1 < y+r and vel[1] == 0:
            loc = _nearest_wall(i, j, 3)
            if any(a in edges for a in loc):
                pos[1] -= r
        elif y-r < j < y and vel[1] == 0:
            loc = _nearest_wall(i, j, 4)
            if any(a in edges for a in loc):
                pos[1] += r
    else:
        if y < j+1 < y+r and vel[l] > 0:
            loc = _nearest_wall(i, j, 3)
            if any(a in edges for a in loc):
                vel[l] *= -1
        elif y-r < j < y and vel[l] < 0:
            loc = _nearest_wall(i, j, 4)
            if any(a in edges for a in loc):
                vel[l] *= -1
        if x < i+1 < x+r and vel[0] == 0:
            loc = _nearest_wall(i, j, 1)
            if any(a in edges for a in loc):
                pos[0] -= r
        elif x-r < i < x and vel[0] == 0:
            loc = _nearest_wall(i, j, 2)
            if any(a in edges for a in loc):
                pos[0] += r
    return pos, vel[l]


def pull_apart(pos: list, vel: list, r: float, boxs: list, pairs: list, n_ix: list) -> tuple:
    """
    Another bug fix:
    - if, by chance, two disks are overlapping,
      pull them apart
    """
    a, b = pairs[n_ix - len(boxs)]
    for k in range(2):
        if pos[a][k] > pos[b][k]:
            pos[a][k] += r
            pos[b][k] -= r
        else:
            pos[a][k] -= r
            pos[b][k] += r
    return pos, vel


def simulate_step(p, v, r, bx, pr, edges, t, next_e, next_e_i, dt=0):
    """
    Run one step of simulation
    """
    if dt:
        next_t = t + dt
    else:
        next_t = t + next_e
    q = 0
    v_old = v
    while t+next_e <= next_t:
        if q > 100 and all(abs(v[k][l]) == abs(v_old[k][l]) for k, l in bx):

            # clearly, we're just stuck here, doing nothing,
            # so just skip to next event
            step = max(dt, next_e)
        else:

            # but if not, iterate over the smallest timestep
            step = min(dt, next_e)
        q += 1
        t += step
        for k, l in bx:
            p[k], v[k][l] = fix_delta(p[k], v[k], r, edges, l)
            p[k][l] += v[k][l]*step
        v = get_velocities(p, v, bx, pr, next_e_i)
        next_e, next_e_i = get_next_event(p, v, bx, pr, r, edges)
        if next_e < 0 and next_e_i > len(bx):
            p, v = pull_apart(p, v, r, bx, pr, next_e_i)
        v_old = v
    remain_t = next_t - t
    for k, l in bx:
        p[k], v[k][l] = fix_delta(p[k], v[k], r, edges, l)
        p[k][l] += v[k][l]*remain_t
    t += remain_t
    next_e -= remain_t
    return p, v, next_e, next_e_i, t


def run_simulation(n, p, v, r, edges, n_events, dt, stepsize, out, logfile):
    """
    Run Molecular Dynamics simulation
    """
    # find the exit point
    max_x = max(x[1][0] for x in edges)

    bx, pr = get_bx_pr(n)

    if os.path.isdir(out):
        print("I: Output directory already exists, deleting any files within it...")
        for file in os.listdir(out):
            os.remove(f"{out}/{file}")
    t, i = 0, 0

    next_e, next_e_i = get_next_event(p, v, bx, pr, r, edges)
    save_log(logfile, t, i, p, v)
    i += 1
    for _ in range(n_events):
        p, v, next_e, next_e_i, t = simulate_step(p, v, r, bx, pr, edges, t, next_e, next_e_i, dt)

        if not i%stepsize:
            save_log(logfile, t, i//stepsize, p, v)
        i += 1
        if not i%(stepsize//10):
            print(f"\033[KI: Simulating timestep {t:.5f} s\r", end='', flush=True)

        # check if any particle has reached the exit
        for pos in p:
            if max_x-2 < pos[0] < max_x+1 and pos[1]+r < 0:
                print(f"\nI: Timestep {t:.5f}; a particle solved the maze! Halting")
                return t, p, v, 1
    print(f"\nI: Finished simulation for {t} timesteps")
    return t, p, v, 0


def simulation_with_fan(n, orig_n, p, v, r, t, edges, n_events, fan_speed, height, dt, stepsize, out, logfile):
    """
    Run Molecular Dynamics simulation with pressure gradient
    """
    # find the exit point
    max_x = max(x[1][0] for x in edges)

    bx, pr = get_bx_pr(n)

    if os.path.isdir(out):
        print("I: Output directory already exists, deleting any files within it...")
        for file in os.listdir(out):
            os.remove(f"{out}/{file}")
    t, i = 0, 0

    next_e, next_e_i = get_next_event(p, v, bx, pr, r, edges)
    save_log(logfile, t, i, p, v)
    i += 1
    for _ in range(n_events):
        p, v, next_e, next_e_i, t = simulate_step(p, v, r, bx, pr, edges, t, next_e, next_e_i, dt)

        if not i%stepsize:
            save_log(logfile, t, i//stepsize, p, v)

            # add new particles to the mix
            if uniform(0, 1) < fan_speed/10:
                p_a, v_a = particle_shower([p, v], r, t, height, orig_n)
                if p_a[-1][0] != 0:
                    p, v = p_a, v_a
                else:
                    p, v = p_a[:-1], v_a[:-1]
                n = len(p)
                bx, pr = get_bx_pr(n)
                next_e, next_e_i = get_next_event(p, v, bx, pr, r, edges)
        i += 1

        # //13 (or any other prime number, I guess) makes the output look more...busy :P
        if not i%(stepsize//13):
            print(f"\033[KI: Simulating timestep {t:.5f} s ({n} particles)\r", end='', flush=True)

        # check if any particle has reached the exit
        for pos in p:
            if max_x-2 < pos[0] < max_x+1 and pos[1]+r < 0:
                print(f"\nI: Timestep {t:.5f}; a particle solved the maze! Halting")
                return t, p, v, n, 1

    print(f"\nI: Finished simulation for {t} timesteps")
    return t, p, v, n, 0
