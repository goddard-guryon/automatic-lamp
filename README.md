# automatic-lamp
Gas diffusion in a 2D maze; inspired by the Numberphile video _The Dumbest Way To Solve A Maze_.

https://user-images.githubusercontent.com/9369053/190667877-9b85bccc-0d3b-4e79-9166-d1e4a75e8596.mov

https://user-images.githubusercontent.com/9369053/191227845-f6624020-1b79-46fd-b267-00dfab5ced68.mov

This version differs from Matt Henderson's algorirthm shown in the video in a couple of ways:

 - Instead of simple Brownian motion, this version does not randomly change the velocity of each particle at every timestep. Instead, it allows the particles to collide with each other, thereby implementing a more accurate version of Brownian motion. However, this does slow down the simulation considerably (the complexity of each step, as far as I can tell, should be $O(n!)$ due to pair collisions between $n$ particles).
 - The simulation is derived from the original Event-driven Molecular Dynamics algorithm by Alder & Wainwright (a simplified version of the original algorithm is used here; this only considers a single type of collision between particles, and considers no attractive or repulsive force between them).
 
**Update:** The code now supports a (rudimentary) way of adding pressure to the maze. The pressure value can be added as a pressure factor in the input, whose value goes from 1 to 10 (0 is equivalent to the original simulation without pressure); lower pressure factor simply adds a new particle/pushes an existing particle less frequently. However, since the algorithm itself runs on the order of $O(n!)$, keep in mind that a higher pressure factor would make the simulation very slow very quickly. I tried to compensate this partly by letting the algorithm pick between adding a new particle at the entry point and pushing an existing particle in the entry box (as an apparent force): when the ratio of original number of particles to current number of particles becomes 1/2, the code only pushes existing particles within the entry box (unless the entry box is already empty, in which case there is no choice but to add another particle). This decreases the frequency of particle addition, but the code can still get very slow as the initial number of particles and/or the grid size increase.
 
 The initial positions are sampled randomly in the 1x1 grid box (_side note:_ I used sequential deposition method instead of Monte Carlo to make this step faster. This, however, means that the step is pseudorandom in some sense), and initial velocities are sampled from the Maxwell distribution.
 
 (The code still isn't 100% bug-free; the major issue is that when a particle is about the hit an open corner of a wall from a very steep angle, it sometimes passes through the wall instead of getting reflected. This is especially more visible when the particle passes in such a way that the center of the particle never overlaps with the wall, only the perimeter of it does).

To run this program, simply clone the repository on your computer, open a terminal in the corresponding directory and type `python main.py`. The program allows modification of some of the parameters of simulation; type `python main.py --help` to get a list of all options. Snapshots are saved by default in the `simulation_snapshots` directory, and a video of the final simulation is saved in the same folder (make sure you have `python-opencv-headless`, and preferably `ffmpegcv`, installed before running the program). The maze structure, particle positions, and particle velocities can be saved in separate files and used again for continuing simulation from where it ended.
