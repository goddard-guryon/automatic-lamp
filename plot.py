"""
Plot a snapshot of the simulation
"""
import os
import matplotlib.pyplot as plt
import cv2


def save_snap(i, p, v, r, edges, output_dir, with_arrows, arrow_scale=.2, e_x=0, e_y=0):
    """
    Save a snapshot of simulation
    """
    lim_x, lim_y = max(x[0][0] for x in edges), max(x[0][1] for x in edges)
    if not os.path.exists(output_dir):
        os.mkdir(output_dir)
    plt.clf()
    plt.xticks([])
    plt.yticks([])
    plt.xlim(-.5, lim_x+.5)
    plt.ylim(-.5, lim_y+.5)
    for e in edges:
        plt.plot([e[0][0], e[1][0]], [e[0][1], e[1][1]], "b-")
    for (x, y), (dx, dy) in zip(p, v):
        dx *= arrow_scale
        dy *= arrow_scale
        col = 'b'
        if e_x-2 < x < e_x+1 and y-r < e_y:
            col = 'r'
        circle = plt.Circle((x, y), radius=r, fc=col)
        plt.gca().add_patch(circle)
        if with_arrows:
            plt.arrow(x, y, dx, dy, fc='k', ec='k', head_length=.05, head_width=.05)
    plt.savefig(os.path.join(output_dir, f"{i}.png"))
    return 0


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
    tpr = []
    for j in range(len(pos)):
        tpr.append((len(pos) - sum(1 if abs(pos[j][i][0]-pos[k][i][0])<3*r and \
                                   abs(pos[j][i][1]-pos[k][i][1])<3*r and j != k else 0 \
                                   for k in range(len(pos))))/len(pos))

    # scale transparency between .1 and 1
    tpr = [.9*(x-min(tpr))/(max(tpr)-min(tpr)) + .1 for x in tpr]
    for j in range(len(pos)-1):
        plt.plot([pos[j][i][0], pos[j+1][i][0]], [pos[j][i][1], pos[j+1][i][1]], "g3-", alpha=tpr[j])
    plt.savefig(os.path.join(output_dir, "trace.png"), dpi=300)
    return 0


def make_video(out):
    """
    Merge all snapshots to create simulation video
    """
    imgs = sorted([f for f in os.listdir(out) if f.endswith(".png")],
                  key=lambda x: int(x.split('.')[0]))
    height, width, _ = cv2.imread(f"{out}/{imgs[0]}").shape
    video = cv2.VideoWriter(f"{out}/final_simulation.avi", 0, 20, (width, height))
    for img in imgs:
        video.write(cv2.imread(f"{out}/{img}"))
    video.release()
    return 0
