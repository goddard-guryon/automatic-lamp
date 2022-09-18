"""
Interface file
"""
from sys import argv
from argparse import ArgumentParser, MetavarTypeHelpFormatter
from wrapper import MazeDiffusion


cli = ArgumentParser(description="Gas Diffusion in a 2D Maze",
                     epilog="More information at https://github.com/goddard-guryon/automatic-lamp",
                     formatter_class=MetavarTypeHelpFormatter)
cli.add_argument("--num",
                 default=10, type=int,
                 help="Number of particles to simulate (default: 10)")
cli.add_argument("--height",
                 default=10, type=int,
                 help="Height of maze (default: 10)")
cli.add_argument("--width",
                 default=10, type=int,
                 help="Width of maze (default: 10)")
cli.add_argument("--duration",
                 default=1e10, type=float,
                 help="""Number of simulation steps.
                 One second corresponds to 20,000 steps with default dt value (default: 1e10)""")
cli.add_argument("--stepsize",
                 default=2000, type=int,
                 help="Save plots every x iterations (default: 2000)")
cli.add_argument("--dt",
                 default=5e-5, type=float,
                 help="Size of one simulation step (default: 5e-5)")
cli.add_argument("--logfile",
                 default="simulation.log", type=str,
                 help="Name of log file (default: 'simulation.log')")
cli.add_argument("--snapdir",
                 default="simulation_snapshots", type=str,
                 help="Directory to save simulation snapshots in (default: 'simulation_snapshots')")
cli.add_argument("--arrows",
                 default=False, type=bool,
                 help="Whether to draw velocity arrows in simulation snapshots (default: False)")
cli.add_argument("--pos_i",
                 default=None, type=str,
                 help="File containing initial positions of particles to import from (default: None)")
cli.add_argument("--vel_i",
                 default=None, type=str,
                 help="File containing initial velocities of particles to import from (default: None)")
cli.add_argument("--maze_i",
                 default=None, type=str,
                 help="File containing maze wall coordinates to import from (default: None)")
cli.add_argument("--pos_o",
                 default=None, type=str,
                 help="Filename to save particle positions in after simulation (default: None)")
cli.add_argument("--vel_o",
                 default=None, type=str,
                 help="Filename to save particle velocities in after simulation (default: None)")
cli.add_argument("--maze_o",
                 default=None, type=str,
                 help="Filename to save maze wall coordinates in (default: None)")
cli.add_argument("--no_sim",
                 default=False, type=bool,
                 help="Skip any simulation (default: False)")
cli.add_argument("--no_snap",
                 default=False, type=bool,
                 help="Skip saving any simulation snapshots (default: False)")
cli.add_argument("--no_trace",
                 default=False, type=bool,
                 help="Skip saving the trace path of exiting particle (default: False)")


args = cli.parse_args(args=None if argv[1:] else ["--help"])
print(cli.description)
cli.print_usage()
print()
arg_dct = {"dt": args.dt,
           "logfile": args.logfile,
           "snapdir": args.snapdir,
           "stepsize": args.stepsize,
           "with_arrows": args.arrows,
           "maze": args.maze_i,
           "pos": args.pos_i,
           "vel": args.vel_i}
instance = MazeDiffusion(args.num, args.height, args.width, **arg_dct)
if not args.no_sim:
    instance.simulate(args.duration)
print()
print(instance)
print()
if not args.no_snap:
    instance.make_snaps()
if not args.no_trace:
    instance.trace_path()
if args.pos_o:
    instance.save_pos(args.pos_o)
if args.vel_o:
    instance.save_vel(args.vel_o)
if args.maze_o:
    instance.save_maze(args.maze_o)
print(f"\n{cli.epilog}\n")
