# automatic-lamp
Gas diffusion in a 2D maze; inspired by the Numberphile video _The Dumbest Way To Solve A Maze_. This version differs from Matt Henderson's algorirthm shown in the video in a couple of ways:

 - Instead of simple Brownian motion, this version does not randomly change the velocity of each particle at every timestep. Instead, it allows the particles to collide with each other, thereby implementing a more accurate version of Brownian motion. However, this does slow down the simulation considerably (the complexity of each step, as far as I can tell, should be $O(n!)$ due to pair collisions between $n$ particles).
 - The simulation is derived from the original Event-driven Molecular Dynamics algorithm by Alder & Wainwright (a simplified version of the original algorithm is used here; this only considers a single type of collision between particles, and considers no attractive or repulsive force between them).
 
 The initial positions are sampled randomly in the 1x1 grid box (_side note:_ I used sequential deposition method instead of Monte Carlo to make this step faster. This, however, means that the step is pseudorandom in some sense), and initial velocities are sampled from the Maxwell distribution.
 
 (The code seems a little buggy since some particles, at certain time steps, get reflected by a wall which doesn't exist in the maze. I've tried to add measures to prevent it, but I'm not sure if those measures will prevent this 100% of the time as, due to the long time of simulation, I can't test every change in the code in multiple runs).
