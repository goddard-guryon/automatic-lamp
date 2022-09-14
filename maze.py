"""
Create a 2D maze
"""
from random import uniform, choice


def fix_path(n, is_in_path, conns):
    """
    Fix path on grid to add any missing nodes
    """
    while not all(is_in_path.values()):
        strays = [k for k, v in is_in_path.items() if not v]
        for cur in strays:

            # maybe we already passed through this box in an
            # earlier iteration of this same loop
            if not is_in_path[cur]:
                connected = False
                c = cur
                while not connected:
                    nbrs = [c+1, c-1, c+n, c-n]
                    nbrs = [x for x in nbrs if x in is_in_path]
                    if not c%n:
                        nbrs.remove(c+1)
                    elif c%n == 1:
                        nbrs.remove(c-1)
                    pick = choice(nbrs)
                    conns[c].append(pick)
                    conns[pick].append(c)
                    is_in_path[c] = True
                    if is_in_path[pick]:
                        connected = True
                    else:
                        c = pick
    return conns


def create_path(m, n):
    """
    Create a path through a grid
    """
    is_in_path = {x: False for x in range(1, m*n+1)}
    conns = {x: [] for x in is_in_path}
    current = [1]
    is_in_path[1] = True
    check = 0

    # create a main path
    while current:
        check += 1
        cur = current.pop()
        nbrs = [cur+1, cur-1, cur+n, cur-n]

        # if you're at the edge of the grid, don't fall off
        if not cur%n:
            nbrs.remove(cur+1)
        elif cur%n == 1:
            nbrs.remove(cur-1)
        nbrs = [x for x in nbrs if x in is_in_path]

        # since we don't want to go in every possible direction
        do_more = uniform(0, 1)
        while do_more < .99 and nbrs:
            nxt = choice(nbrs)
            nbrs.remove(nxt)
            if not is_in_path[nxt]:
                conns[cur].append(nxt)
                conns[nxt].append(cur)
                is_in_path[nxt] = True
                is_in_path[cur] = True
                current.append(nxt)
            do_more = uniform(0, 1)

        # in case we run into infinite loop, run the function again
        if check > 1e4:
            return create_path(m, n)

    # then add any missed boxes to path
    conns = fix_path(n, is_in_path, conns)

    # that's it
    return conns


def maze_grid(m, n):
    """
    Create 2D grid
    """
    # instantiate variables
    nodes, edges = {}, []

    # first set up nodes
    for i in range(m+1):
        nodes[i] = {}
        for j in range(n+1):
            nodes[i][j] = (i, j)

    # create path through the maze
    conns = create_path(m, n)

    # add edges at the border of the maze
    for i in range(m):
        edges.append((nodes[i][0], nodes[i+1][0]))
        edges.append((nodes[i][n], nodes[i+1][n]))
    for i in range(n):
        edges.append((nodes[0][i], nodes[0][i+1]))
        edges.append((nodes[m][i], nodes[m][i+1]))

    # now add edges so as to preserve path
    for b in range(1, m*n+1):
        i, j = b//n+1, b%n
        if not j:
            j = n
            i -= 1
        box = {b-n: (nodes[i-1][j-1], nodes[i-1][j]),
               b-1: (nodes[i-1][j-1], nodes[i][j-1]),
               b+1: (nodes[i-1][j], nodes[i][j]),
               b+n: (nodes[i][j-1], nodes[i][j])}
        for nbr, wall in box.items():
            if nbr not in conns[b]:
                edges.append(wall)

    # done
    return edges


def make_maze(m, n=0):
    """
    Main function to make 2D maze
    """
    # all that's left is adding an entry and exit point
    if not n:
        n = m
    edges = maze_grid(m, n)

    # entry point
    edges.append(((0, m), (0, m+1)))
    edges.append(((0, m+1), (1, m+1)))
    edges.append(((1, m+1), (1, m)))
    edges.append(((0, m+1), (0, m)))
    edges.append(((1, m+1), (0, m+1)))
    edges.append(((1, m), (1, m+1)))

    # make way
    for rem in [((0, m), (1, m)), ((1, m), (0, m)), ((n-1, 0), (n, 0)), ((n, 0), (n-1, 0))]:
        while rem in edges:
            edges.remove(rem)

    return edges
