"""
Create a 2D maze
"""
from random import uniform, choice
import matplotlib.pyplot as plt


def make_maze(m, n=0):
    # instantiate variables
    check = 0
    nodes, edges = {}, []
    if not n:
        n = m

    # first set up nodes
    for i in range(m+1):
        nodes[i] = {}
        for j in range(n+1):
            nodes[i][j] = (i, j)

    # create a path through the maze first
    is_in_path = {x: False for x in range(1, m*n+1)}
    conns = {x: [] for x in is_in_path}
    current = [1]
    is_in_path[1] = True

    # create a main path
    while current:
        check += 1
        cur = current.pop()
        nbrs = [cur+1, cur-1, cur+n, cur-n]
        if not (cur+1)%n:
            nbrs.remove(cur+1)
        if not (cur-1)%n:
            nbrs.remove(cur-1)
        nbrs = [x for x in nbrs if x in is_in_path]
        do_more = uniform(0, 1)
        while do_more < .9 and nbrs:
            nxt = choice(nbrs)
            nbrs.remove(nxt)
            if not is_in_path[nxt]:
                conns[cur].append(nxt)
                conns[nxt].append(cur)
                is_in_path[nxt] = True
                is_in_path[cur] = True
                current.append(nxt)
            do_more = uniform(0, 1)
        if check > 1e4:
            return 1

    # then add any missed boxes to path
    while not all(is_in_path.values()):
        strays = [k for k, v in is_in_path.items() if not v]
        for cur in strays:
            nbrs = [cur+1, cur-1, cur+n, cur-n]
            nbrs = [x for x in nbrs if x in is_in_path]
            if not (cur+1)%n:
                nbrs.remove(cur+1)
            if not (cur-1)%n:
                nbrs.remove(cur-1)
            pick = choice(nbrs)
            conns[cur].append(pick)
            conns[pick].append(cur)
            is_in_path[cur] = True

    # add edges at the border of the maze
    for i in range(m):
        edges.append((nodes[i][0], nodes[i+1][0]))
        edges.append((nodes[i][n], nodes[i+1][n]))
    for i in range(n):
        edges.append((nodes[0][i], nodes[0][i+1]))
        edges.append((nodes[m][i], nodes[m][i+1]))

    # now add edges so as to preserve path
    for b in range(1, m*n):
        i, j = b//n+1, b%n
        if not j:
            j = n
        box = {b-n: (nodes[i-1][j-1], nodes[i-1][j]),
               b-1: (nodes[i-1][j-1], nodes[i][j-1]),
               b+1: (nodes[i-1][j], nodes[i][j]),
               b+n: (nodes[i][j-1], nodes[i][j])}
        if i == n:
            del box[b+n]
        for nbr, wall in box.items():
            if nbr not in conns[b]:
                edges.append(wall)

    # idk why, but the top row of maze is messing up
    for i in range(1, n):
        box = {"left": (nodes[i-1][n-1], nodes[i-1][n]),
               "down": (nodes[i-1][n-1], nodes[i][n-1]),
               "right": (nodes[i][n-1], nodes[i][n]),
               "up": (nodes[i-1][n-1], nodes[i][n])}
        if box["left"] in edges and box["right"] in edges and box["down"] in edges:
            edges.remove(box["down"])
        elif box["down"] in edges and uniform(0, 1) < .5:
            edges.remove(box["down"])

    # done
    return edges
