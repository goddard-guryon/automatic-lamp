"""
Plot a snapshot of the simulation
"""
import os
import matplotlib.pyplot as plt
try:
    from cv2 import imread
    runnable = True
except ModuleNotFoundError:
    print("W: Failed to import opencv; simulation video generation will be disabled.")
    RUNNABLE = False
try:
    from ffmpegcv import VideoWriter
    FALLBACK = False
except ModuleNotFoundError:
    print("W: Failed to load ffmpegcv; falling back to opencv.")
    print("W: Simulation video size may become too large; ffmpegcv installation is recommanded")
    from cv2 import VideoWriter
    FALLBACK = True


def save_snap(edges):
    """
    Save a snapshot of simulation
    """
    plt.clf()
    for e in edges:
        plt.plot([e[0][0], e[0][1]], [e[1][0], e[1][1]], "b-")
    plt.show()
    return 0

edges = []
n = 6
with open("edges.txt", 'r') as file:
    edge, tmp = [], []
    for line in file.readlines():
        if line.startswith('['):
            fw = line.rstrip()[2:-2].split(') => (')
            fw = [tuple(int(y) for y in x.split(',')) for x in fw]
            edges.append(fw)
        else:
            print(line)
save_snap(edges)


def plot_trace_path(pos, w, r, edges, output_dir):
    """
    Plot the path of the particle that solved the maze
    """
    if not os.path.exists(output_dir):
        os.mkdir(output_dir)
    i = min(p[1] for p in pos[-1] if w-2 < p[0] < w)
    i = pos[-1].index([x for x in pos[-1] if x[1] == i][0])
    lim_x, lim_y = max(x[0][0] for x in edges), max(x[0][1] for x in edges)
    plt.clf()
    plt.xticks([])
    plt.yticks([])
    plt.xlim(-.5, lim_x+.5)
    plt.ylim(-.5, lim_y+.5)
    for e in edges:
        plt.plot([e[0][0], e[1][0]], [e[0][1], e[1][1]], "b-")

    # change transparency by density of points around every point
    tp = []
    for j in range(len(pos)):
        tp.append((len(pos) - sum(1 if abs(pos[j][i][0]-pos[k][i][0])<3*r and \
                                  abs(pos[j][i][1]-pos[k][i][1])<3*r and j != k else 0 \
                                  for k in range(len(pos))))/len(pos))

    # scale transparency between .1 and .99
    tp = [(.99-.1)*(x-min(tp))/(max(tp)-min(tp)) + .1 for x in tp]
    for j in range(len(pos)-1):
        plt.arrow(pos[j][i][0],
                  pos[j][i][1],
                  pos[j+1][i][0]-pos[j][i][0],
                  pos[j+1][i][1]-pos[j][i][1],
                  alpha=tp[j],
                  fc='g',
                  length_includes_head=True,
                  head_width=.05)
    plt.savefig(os.path.join(output_dir, "trace.png"), dpi=300)
    return 0


def make_video(out):
    """
    Merge all snapshots to create simulation video
    """
    if not runnable:
        print("E: Failed to import opencv; video generation has been disabled.")
        print("I: Please install python-opencv-headless/python-opencv to enable video generation.")
        return 1
    imgs = sorted([f for f in os.listdir(out) if f.endswith(".png")],
                  key=lambda x: int(x.split('.')[0]))
    ht, wd, _ = imread(f"{out}/{imgs[0]}").shape
    if not FALLBACK:
        video = VideoWriter(f"{out}/final_simulation.avi", None, 20, (wd, ht), pix_fmt="bgr24")
    else:
        video = VideoWriter(f"{out}/final_simulation.avi", 0, 20, (wd, ht))
    for img in imgs:
        frame = imread(f"{out}/{img}")
        video.write(frame)
    video.release()
    return 0
