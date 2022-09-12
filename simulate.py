"""
EdMD simulation
"""
from maze import make_maze
import matplotlib.pyplot as plt


def wall_time(x, v, r):
    """
    Time before a particle hits a wall
    """
    # maybe find a value k, which is the number
    # of blocks which are empty between disk and next wall.
    # eg: if only empty blocks for next 3 blocks, then a wall,
    # then k=3. Then return (k-r-x)/v
    if v > 0:
        return (1-r-x)/v
    if v == 0:
        return (x-r)/abs(v)
    return float("inf")


def get_next_event(pos, vel, singles, s):
    """
    Find which event happens next
    """
    hits = [wall_time(pos[k][l], vel[k][l], s) for k, l in singles]
    return min(zip(hits, range(len(hits))))


def get_velocities(pos, vel, singles, n_ix, walls):
    """
    Change velocities after event
    """
    disk, direction = singles[n_ix]


grid_size = 19
edges = make_maze(grid_size)
for edge in edges:
    plt.plot([edge[0][0], edge[1][0]], [edge[0][1], edge[1][1]], "b-")
plt.show()
