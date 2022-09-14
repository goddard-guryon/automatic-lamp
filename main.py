"""
Main file
"""
from math import sqrt, pi
from maze import make_maze
from initialize import initial_pos, initial_vel
from simulate import run_simulation


# take values
n = int(input("Number of disks (default 10): ") or 10)
height = int(input("Maze height (default 10): ") or 10)
width = int(input("Maze width (default 10): ") or 10)
duration = int(input("Duration of simulation (default 1M steps or 50 sec): ") or 1e6)
print("Initializing")

# determine appropriate radius, so that disks don't
# overlap each other
radius = sqrt(.2/n/pi)

# create maze
edges = make_maze(height, width)

# spawn the disks
positions = initial_pos(n, radius)
velocities = initial_vel(n)

# shift the disks to entry box
positions = [[p[0], p[1]+height] for p in positions]

# start simulation
print("Beginning simulation")
run_simulation(n, positions, velocities, radius, edges, duration)
