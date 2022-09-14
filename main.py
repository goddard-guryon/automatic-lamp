"""
Main file
"""
import os
from math import sqrt, pi
from maze import make_maze
from initialize import initial_pos, initial_vel
from simulate import run_simulation


# take values
n = int(input("Number of disks (default 10): ") or 10)
height = int(input("Maze height (default 10): ") or 10)
width = int(input("Maze width (default 10): ") or 10)
duration = int(float(input("Duration of simulation (default 10B steps/5.787 days; 1 sec = 20k steps): ") or 1e10))
print("Initializing")

# determine appropriate radius, so that disks don't
# overlap each other
radius = sqrt(.3/n/pi)

# create maze
edges = make_maze(height, width)

# spawn the disks
positions = initial_pos(n, radius)
velocities = initial_vel(n)

# shift the disks to entry box
positions = [[p[0], p[1]+height] for p in positions]

if os.path.exists("simulation.log"):
    os.remove("simulation.log")
with open("simulation.log", 'w') as file:
    file.write('\n')

# start simulation
print("Beginning simulation")
run_simulation(n, positions, velocities, radius, edges, duration)
