"""
Plot a snapshot of the simulation
"""
import os
import matplotlib.pyplot as plt
import cv2


def save_snap(i, p, v, r, edges, output_dir, with_arrows, arrow_scale=.2, ex=None, ey=None):
    """
    Save a snapshot of simulation
    """
    lim = max(x[0][0] for x in edges)
    if not os.path.exists(output_dir):
        os.mkdir(output_dir)
    plt.clf()
    plt.xticks([])
    plt.yticks([])
    plt.xlim(-.5, lim+.5)
    plt.ylim(-.5, lim+1.5)
    for e in edges:
        plt.plot([e[0][0], e[1][0]], [e[0][1], e[1][1]], "b-")
    for (x, y), (dx, dy) in zip(p, v):
        dx *= arrow_scale
        dy *= arrow_scale
        col = 'b'
        if ex or ey:
            if ex-1 < x < ex and y+r < ey:
                col = 'r'
        circle = plt.Circle((x, y), radius=r, fc=col)
        plt.gca().add_patch(circle)
        if with_arrows:
            plt.arrow(x, y, dx, dy, fc='k', ec='k', head_length=.05, head_width=.05)
    plt.savefig(os.path.join(output_dir, f"{i}.png"))
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
