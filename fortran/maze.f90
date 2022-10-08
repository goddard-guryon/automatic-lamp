!> @brief Maze generation for simulation
!> This module contains functions for creating a maze object.
!> The maze path is generated as a tree, and the function
!> returns a list of edges between the nodes of the grid.
module maze
    implicit none

    !> @brief node object for tree construction
    !> Only used for generation of the tree for maze construction
    !> @param id ID of the node
    !> @param n_nbrs Number of connected neighbors of the node
    !> @param nbrs Array of IDs of connected neighbors
    type node_t
        private
        integer, public :: id

        ! was initially using pointers, but this is just easier
        integer :: n_nbrs = 0, nbrs(4) = 0
    end type node_t
contains

    !> @brief get neighboring boxes in the grid
    !> Given a box (=node) location, find its neighboring
    !> boxes in the grid.
    !> @param k Location of node (int from 1 to m*n)
    !> @param m number of rows in grid
    !> @param n number of columns in grid
    function get_nbrs(k, m, n) result(nbrs)
        implicit none
        integer, intent(in) :: k, m, n
        integer :: nbrs(4), i, tmp
        real :: rnd
        nbrs = 0
        i = 1

        ! check left-right first
        if (mod(k, n) /= 0) then
            nbrs(i) = k + 1
            i = i + 1
        end if
        if (mod(k, n) /= 1) then
            nbrs(i) = k - 1
            i = i + 1
        end if

        ! now check top-bottom
        if (k > m .and. mod(k, m) /= 1) then
            nbrs(i) = k - n
            i = i + 1
        end if

        if (k+n < m*n .and. mod(k, m) /= 0) then
            nbrs(i) = k + n
            i = i + 1
        end if

        ! need to shuffle the neighbors, to add more randomness to
        ! maze structure; thus, Fisher-Yates
        do i = 1, size(nbrs)-1
            tmp = nbrs(i)
            call random_number(rnd)
            rnd = (size(nbrs)-i)*rnd + i  ! transpose [0, 1] => [i, size(nbrs)]
            nbrs(i) = nbrs(int(rnd))
            nbrs(int(rnd)) = tmp
        end do
    end function get_nbrs

    !> @brief Add a child node
    !> Small function to connect two nodes to each other
    !> and create a node if only one of the two exists.
    !> @param tree The array of nodes already created
    !> @param parent ID of the extending node
    !> @param nbr_id ID of the node being connected to
    !> @return child The newly connected node
    function add_child(tree, parent, nbr_id) result(child)
        implicit none
        type(node_t), intent(inout), target :: tree(:), parent
        integer, intent(in) :: nbr_id
        type(node_t), target :: child

        if (tree(nbr_id)%n_nbrs == 0) then
            child = node_t(id=nbr_id, n_nbrs=1)
            child%nbrs(1) = parent%id
        else
            child = tree(nbr_id)
            if (child%nbrs(2) /= 0) then
                if (child%nbrs(3) /= 0) then
                    child%nbrs(4) = parent%id
                else
                    child%nbrs(3) = parent%id
                end if
            else
                child%nbrs(2) = parent%id
            end if
        end if
        if (parent%nbrs(2) /= 0) then
            if (parent%nbrs(3) /= 0) then
                parent%nbrs(4) = child%id
            else
                parent%nbrs(3) = child%id
            end if
        else
            parent%nbrs(2) = child%id
        end if
    end function add_child


    !> @brief add any orphan nodes into tree
    !> If there are any nodes in the tree which haven't been
    !> visited yet, connect them to their nearest neighbor
    !> that has been visited. Supporting suburoutine for maze_grid function.
    !> @param m Height of maze
    !> @param n Width of maze
    !> @param tree Preliminary tree structure
    subroutine fix_grid(m, n, tree)
        implicit none
        type(node_t), intent(inout) :: tree(m*n)
        integer, intent(in) :: m, n
        integer :: i, j, nbrs(4)

        ! we may need multiple passes
        guard: do while (.true.)

            ! iterate over current tree structure
            do i = 1, size(tree)

                ! if any node is unconnected
                if (tree(i)%n_nbrs == 0) then

                    ! create a node here
                    tree(i) = node_t(id=i, n_nbrs=0)

                    ! find its nearest connected neighbor
                    nbrs = get_nbrs(i, m, n)
                    joiner: do j = 1, size(nbrs)

                        ! if this neighbor is connected, join to it
                        if (nbrs(j) /= 0) then
                            if (tree(nbrs(j))%id /= 0) then
                                tree(nbrs(j)) = add_child(tree, tree(i), nbrs(j))
                                tree(i)%n_nbrs = tree(i)%n_nbrs + 1

                                ! no need to join multiple times
                                exit joiner
                            end if
                        end if
                    end do joiner
                end if
            end do

            ! if all nodes have been joined, leave
            do i = 1, size(tree)
                if (tree(i)%n_nbrs == 0) exit
            end do
            if (i >= size(tree)) exit guard
        end do guard
    end subroutine fix_grid

    !> @brief Generation of maze path as a tree
    !> This generates the tree object as an array of nodes
    !> which corresponds to a path through the maze.
    !> @param m The number of rows (height) of maze
    !> @param n The number of columns (width) of maze
    !> @return tree Array of node_t objects
    function maze_grid(m, n) result(tree)
        implicit none
        integer, intent(in) :: m, n
        real, parameter :: strength = 0.1  ! 0-1; higher value -> maze more open
        integer :: current(m*n), covered(m*n), i, j, nbrs(4), check, root
        type(node_t) :: tree(m*n)
        type(node_t), target :: new_node
        real :: rnd(4)  ! whenever I need it, I need 4 random numbers

        ! safety loop
        guard: do while (.true.)

            !reinitialize
            covered = 0
            current = 0
            do i = 1, size(tree)
                tree(i) = node_t(id=0, n_nbrs=0, nbrs=0)
            end do

            ! start with 4 different roots to make it more random
            call random_number(rnd)
            do i = 1, 4
                root = int(rnd(i)*(m*n)) + 1
                new_node = node_t(id=root, n_nbrs=1)
                new_node%nbrs(1) = root
                tree(root) = new_node
                current(root) = 1
                covered(root) = 1
            end do

            ! check if we're done
            check = 0
            do i = 1, size(tree)
                if (tree(i)%n_nbrs == 0) check = check + 1
            end do
            if (check == 0) exit guard
            check = 0

            ! keep propagating until we have nowhere to go
            prop: do while (any(covered == 0))

                ! we might just get stuck going nowhere, watch out for that too
                check = check + 1

                ! propagate at each block
                do i = 1, size(tree)
                    if (current(i) /= 0) then
                        nbrs = get_nbrs(i, m, n)

                        ! iterate over list of neighbors
                        call random_number(rnd)
                        more: do j = 1, 4

                            ! if we have at least two neighbors, consider leaving
                            if (tree(i)%n_nbrs > 1) then

                                ! strength is just 1 - probability threshold i.e. strength = 0.3
                                ! means at any step, we have 70% chance of leaving
                                if (rnd(j) > strength) then
                                    exit more
                                end if
                            end if

                            ! only connect if target node isn't already connected
                            ! lest there shall be no borders in the world
                            if (nbrs(j) /= 0 .and. all(tree(i)%nbrs /= nbrs(j))) then

                                ! for some reason, I can't even use .and. here :)
                                if (tree(nbrs(j))%n_nbrs <= 3) then
                                    new_node = add_child(tree, tree(i), nbrs(j))
                                    tree(nbrs(j)) = new_node
                                    tree(i)%n_nbrs = tree(i)%n_nbrs + 1
                                    current(nbrs(j)) = 1  ! mark this to be visited
                                    covered(nbrs(j)) = 1
                                end if
                            end if
                        end do more
                        current(i) = 0  ! mark that we've visited this
                    end if
                end do

                ! check if we're stuck, but let it run for sufficiently long first
                if (check > 50*m*n) exit prop
            end do prop

            ! pull in any left out nodes
            call fix_grid(m, n, tree)

            ! let's make sure regardless
            do i = 1, size(tree)
                if (tree(i)%n_nbrs /= 0) then
                    covered(i) = 1
                end if
            end do
            if (all(covered /= 0)) exit guard
        end do guard
    end function maze_grid

    !> @brief Print the list of edges
    !> Simply call this subroutine to print an edge array in a
    !> formatted manner. To print the whole edges object, iterate
    !> over the object and call this subroutine on each item in it.
    !> @param e Edge array (2x2x2 array)
    !> @param extra Extra (optional) string to print after edge
    subroutine f_print_edge(e, extra)
        use, intrinsic :: iso_fortran_env, only : output_unit
        implicit none
        integer, intent(in) :: e(2, 2)
        character(len=*), intent(in), optional :: extra
        character(len=16) :: fmt_str

        if (.not. present(extra)) then
            fmt_str = '(8(A, I0), A)'
            write(unit=output_unit, fmt=fmt_str) '[(', e(1,1), ',', e(1,2), ') => (', e(2,1), ',', &
                e(2,2), ')]'
        else
            fmt_str = '(8(A, I0), A, A)'
            write(unit=output_unit, fmt=fmt_str) '[(', e(1,1), ',', e(1,2), ') => (', e(2,1), ',', &
                e(2,2), ')]'
        end if
    end subroutine
    

    !> @brief Print the tree as list of edges
    !> Simply call this subroutine to print a tree output in a
    !> formatted manner. The tree is printed as list of connections
    !> between nodes.
    !> @param tree Array of node_t objects (m x n array)
    subroutine f_print_tree(tree)
        use, intrinsic :: iso_fortran_env, only : output_unit
        implicit none
        type(node_t), intent(in) :: tree(:)
        integer :: i
        character(len=42) :: fmt_str

        fmt_str = '(I0, A, I0, A, I0, A, I0, A, I0, A, I0, A)'
        do i = 1, size(tree)
            write(unit=output_unit, fmt=fmt_str) tree(i)%id, '(', tree(i)%n_nbrs, ') => (', &
                tree(i)%nbrs(1), ', ', tree(i)%nbrs(2), ', ', tree(i)%nbrs(3), ', ', tree(i)%nbrs(4), ')'
        end do

    end subroutine

    !> @brief get the edge coordinates, given a box in grid
    !> Given the x and y parameters of the box, return the
    !> corresponding wall in the maze, represented as a list of edges.
    !> The return object is represented as a 3D array such that:
    !> [[(x, y of node 1), (x, y of node 2)], [(x, y of node 2), (x, y of node 1)]
    !> (both orders are listed to prevent any orientation mismatch, also see f_print).
    !> @param x The box being considered
    !> @param y The neighboring box
    !> @param n The width of the maze
    !> @return Corresponding wall in the maze, represented as a 3D array
    function get_edge(x, y, n) result(e)
        implicit none
        integer, intent(in) :: x, y, n
        integer :: i, j
        integer :: e(4)

        i = x/n + 1
        j = mod(x, n)
        if (j == 0) j = n

        ! idk why but I can't assign it all in one line /)-_-)
        if (y == x-n) then  ! bottom wall: bottom-left to bottom-right
            e(1) = i-1
            e(2) = j-1 
            e(3) = i-1
            e(4) = j
        else if (y == x+n) then  ! top wall: top-left to top-right
            e(1) = i-1
            e(2) = j
            e(3) = i
            e(4) = j
        else if (y == x-1) then  ! left wall: bottom-left to top-left
            e(1) = i-1
            e(2) = j-1
            e(3) = i-1
            e(4) = j
        else if (y == x+1) then  ! right wall: bottom-right to top-right
            e(1) = i
            e(2) = j-1
            e(3) = i
            e(4) = j
        end if
    end function get_edge

    !> @brief Main function for maze generation
    !> This function generates the maze edges, taking the tree
    !> structure from maze_grid function and converting it into
    !> a list of edges which corresponds to the maze.
    !> @param m Height of maze
    !> @param n Width of maze
    !> @return edges List of edges in 
    function make_maze(m, n) result(edges)
        use, intrinsic :: iso_fortran_env, only : output_unit
        implicit none
        integer, intent(in) :: m, n
        type(node_t) :: tree(m*n)
        integer :: a, i, j, k = 0, l, buf(8*m*n, 4), nbr_lst(4), edge(4)
        integer, allocatable :: edges(:, :)

        tree = maze_grid(m, n)
        k = 1
        do a = 1, size(tree)
            i = tree(a)%id

            ! get all neighbors this node could have
            nbr_lst = get_nbrs(i, m, n)
            do l = 2, 4
                j = nbr_lst(l)

                ! if this neighbor isn't connected, create a wall
                if (.not. any(tree(a)%nbrs == j)) then
                    edge = get_edge(i, j, n)

                    ! forward edge
                    prune_f: do j = 1, a

                        ! don't want to add redundant edges
                        if (all(buf(j, :) == edge)) then
                            exit prune_f
                        end if
                        if (j == k) then
                            buf(k, :) = edge(:)
                            k = k + 1
                            exit prune_f
                        end if
                    end do prune_f

                    edge = [edge(3), edge(4), edge(1), edge(2)]

                    ! it's tradeoff between size and speed, but I prefer size
                    prune_r: do j = 1, a
                        if (all(buf(j, :) == edge)) then
                            exit prune_r
                        end if
                        if (j == k) then
                            buf(k, :) = edge(:)
                            k = k + 1
                            exit prune_r
                        end if
                    end do prune_r
                end if
            end do
        end do

        ! add borders
        do i = 0, m-1

            ! left border
            buf(k, :) = [0, i, 0, i+1]
            buf(k+1, :) = [0, i+1, 0, i]

            ! right border
            buf(k+2, :) = [n, i, n, i+1]
            buf(k+3, :) = [n, i+1, n, i]
            k = k + 4
        end do
        do i = 0, n-1

            ! bottom border
            buf(k, :) = [i, 0, i+1, 0]
            buf(k+1, :) = [i+1, 0, i, 0]

            ! top border
            buf(k+2, :) = [i, m, i+1, m]
            buf(k+3, :) = [i+1, m, i, m]
            k = k + 4
        end do

        ! now create a new array of this size
        allocate(edges(k, 4))
        do i = 1, k
            edges(i, :) = buf(i, :)
        end do
    end function make_maze
end module maze
program main
    use, intrinsic :: iso_fortran_env, only : int16, int32, real32, output_unit
    use :: maze
    implicit none
    integer, allocatable :: output(:, :)
    integer :: i
    output = make_maze(10, 10)
    do i = 1, size(output, 1)
        call f_print_edge(output(i, :))
    end do
end program main
