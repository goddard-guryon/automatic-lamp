"""
Main class definition
"""
import os
from math import sqrt, pi
from maze import make_maze
from initialize import initial_pos, initial_vel
from simulate import run_simulation
from plot import save_snap, make_video


class MazeDiffusion():
    def __init__(self, n=10, height=10, width=10, **kwargs):
        self.n = n
        self.height = height
        self.width = width
        self.radius = 0
        self.pos, self.vel, self.grid = [], [], []
        self.dt = kwargs.get("dt", 5e-5)
        self.duration = kwargs.get("duration", 0)
        self.stepsize = kwargs.get("stepsize", 2000)
        self.with_arrows = kwargs.get("with_arrows", False)
        self.logfile = kwargs.get("logfile", "simulation.log")
        self.snapdir = kwargs.get("snapdir", "simulation_snapshots")
        self.pos = kwargs.get("positions", None)
        self.vel = kwargs.get("velocities", None)
        self.grid = kwargs.get("maze", None)

    def initialize(self):
        """
        Initialize simulation parameters
        """
        print("Initializing system...\r", end='', flush=True)
        self.radius = sqrt(.3/self.n/pi)
        if not self.pos:
            self.pos = initial_pos(self.n, self.radius)
            self.pos = [[p[0], p[1] + self.height] for p in self.pos]
        else:
            self.n = 0
            self.import_pos()
        if not self.vel:
            self.vel = initial_vel(self.n)
        else:
            self.n = 0
            self.import_vel()
        if not self.grid:
            self.grid = make_maze(self.height, self.width)
        else:
            self.import_grid()
        if os.path.exists(self.logfile):
            os.remove(self.logfile)
        with open(self.logfile, 'w') as file:
            file.write("Initialized system\n")
        print("Initializing system...Done")
        return 0

    def import_pos(self):
        """
        Import initial particle positions from file
        """
        print("Importing initial positions...\r", end='', flush=True)
        data, x = [], 0
        with open(self.pos, 'r') as file:
            for line in file.readlines()[1:]:
                data.append([float(x) for x in line.rstrip().split(' ')])
                x += 1
        if self.n not in [0, x]:
            print("\nFailed to import positions; number of particles do not match.")
        else:
            self.pos = data
            self.n = x
        print(f"Imported initial positions for {x} particles")
        return 0

    def import_vel(self):
        """
        Import initial particle velocities from file
        """
        print("Importing initial velocities...\r", end='', flush=True)
        data, x = [], 0
        with open(self.pos, 'r') as file:
            for line in file.readlines()[1:]:
                data.append([float(x) for x in line.rstrip().split(' ')])
                x += 1
        if self.n not in [0, x]:
            print("\nFailed to import velocities; number of particles do not match.")
        else:
            self.vel = data
            self.n = x
        print(f"Imported initial velocities for {x} particles")
        return 0

    def import_grid(self):
        """
        Import maze wall coordinates from file
        """
        print("Importing maze wall coordinates...\r", end='', flush=True)
        data = []
        with open(self.grid, 'r') as file:
            for line in file.readlines()[1:]:
                edge = line.rstrip().split(' -> ')
                from_, to_ = [x.strip('(').strip(')').strip(',') for x in edge]
                from_, to_ = tuple(int(x) for x in from_.split()), tuple(int(x) for x in to_.split())
                data.append((from_, to_))
        self.grid = data
        print("Importing maze wall coordinates...Done")
        return 0

    def make_snaps(self):
        """
        Plot snapshots of simulation
        """
        with open(self.logfile, 'r') as file:
            t, i, p, v = -1, 0, [], []
            for line in file.readlines():
                if line.startswith("time"):
                    if t >= 0:
                        save_snap(i, p, v, self.radius, self.grid,
                                  self.snapdir, self.with_arrows)
                        t, i, p, v = 0, 0, [], []
                    _, t, _, i = line.split(' ')
                    t, i = float(t), int(i)
                    print(f"\033[KPlotting timestep: {t:.5f}\r", end='', flush=True)
                elif line.startswith("pos"):
                    p.append([float(x) for x in line.rstrip().split(' ')[1:]])
                elif line.startswith("vel"):
                    v.append([float(x) for x in line.rstrip().split(' ')[1:]])
            save_snap(i+1, p, v, self.radius, self.grid,
                      self.snapdir, self.with_arrows, ex=max(x[1][0] for x in self.grid), ey=0)
        print("\nMerging snapshots to create final video")
        make_video(self.snapdir)
        print("Done")
        return 0

    def simulate(self, num_steps):
        """
        Run simulation on the system instance
        """
        if not self.radius:
            print("Please initialize the positions and velocities first.")
            return 0
        time = run_simulation(self.n, self.pos, self.vel, self.radius, self.grid, int(num_steps),
                              self.dt, self.stepsize, self.snapdir, self.logfile)
        self.duration += time
        print("Creating visualization plots")
        return 0

    def save_maze(self, grid_file):
        """
        Save maze coordinates to file
        """
        print("Saving maze wall coordinates...\r", end='', flush=True)
        with open(grid_file, 'w') as file:
            file.write("Maze wall coordinates\n")
            for edge in self.grid:
                file.write(f"({edge[0][0]} {edge[0][1]}) -> ({edge[1][0]} {edge[1][1]})\n")
        print("Saving maze wall coordinates...Done")
        return 0

    def save_pos(self, pos_file):
        """
        Save particle positions to file
        """
        print("Saving particle positions...\r", end='', flush=True)
        with open(pos_file, 'w') as file:
            file.write("Particle positions\n")
            for p in self.pos:
                file.write(f"{p[0]} {p[1]}\n")
        print("Saving particle positions...Done")
        return 0

    def save_vel(self, vel_file):
        """
        Save particle velocities to file
        """
        print("Saving particle velocities...\r", end='', flush=True)
        with open(vel_file, 'w') as file:
            file.write("Particle velocities\n")
            for v in self.vel:
                file.write(f"{v[0]} {v[1]}\n")
        print("Saving particle velocities...Done")
        return 0

    def __repr__(self):
        print("MazeDiffusion object")
        print(f"    Maze size: {self.height} x {self.width}")
        if not self.radius:
            return "    Currently unitialized"
        print(f"    Contains {self.n} particles")
        print(f"    MD Simulation has been run for {self.duration:.5f} seconds")
        avg_vel = sqrt((sum(v[0] for v in self.vel)/len(self.vel))**2 +
                       (sum(v[1] for v in self.vel)/len(self.vel))**2)
        return f"    RMS velocity of particles: {avg_vel}"
