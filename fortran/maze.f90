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
        integer :: nbrs(4), i
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

    !> @brief Generation of maze path as a tree
    !> This generates the tree object as an array of nodes
    !> which corresponds to a path through the maze.
    !> @param m The number of rows (height) of maze
    !> @param n The number of columns (width) of maze
    !> @return tree Array of node_t objects
    function maze_grid(m, n) result(tree)
        use, intrinsic :: iso_fortran_env, only : output_unit
        implicit none
        integer, intent(in) :: m, n
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

            ! keep propagating until we have nowhere to go
            do while (any(covered == 0))

                ! propagate at each block
                do i = 1, size(tree)
                    if (current(i) /= 0) then
                        nbrs = get_nbrs(i, m, n)

                        ! iterate over list of neighbors
                        call random_number(rnd)
                        more: do j = 1, 4

                            ! if we have at least two neighbors, we can leave
                            if (tree(i)%n_nbrs > 1) then
                                if (rnd(j) > 0.9) then
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

                ! for some reason, I still need to do this :/
                if (all(covered /= 0)) exit guard
            end do
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
        integer :: e(2, 2)

        i = x/n + 1
        j = mod(x, n)
        if (j == 0) j = n
        !i = x
        !j = y

        ! idk why but I can't assign it all in one line /)-_-)
        if (y == x-n) then  ! bottom wall: bottom-left to bottom-right
            e(1, 1) = i-1
            e(1, 2) = j-1 
            e(2, 1) = i-1
            e(2, 2) = j
        else if (y == x+n) then  ! top wall: top-left to top-right
            e(1, 1) = i-1
            e(1, 2) = j
            e(2, 1) = i
            e(2, 2) = j
        else if (y == x-1) then  ! left wall: bottom-left to top-left
            e(1, 1) = i-1
            e(1, 2) = j-1
            e(2, 1) = i-1
            e(2, 2) = j
        else if (y == x+1) then  ! right wall: bottom-right to top-right
            e(1, 1) = i
            e(1, 2) = j-1
            e(2, 1) = i
            e(2, 2) = j
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
        implicit none
        integer, intent(in) :: m, n
        type(node_t) :: tree(m*n)
        integer :: a, i, j, k = 0, l, buf(8*m*n, 2, 2), edge(2, 2), tmp(2), nbr_lst(4)
        integer, allocatable :: edges(:, :, :)

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
                        if (all(buf(j, :, :) == edge)) then
                            exit prune_f
                        end if
                        if (j == k) then
                            buf(k, :, :) = edge(:, :)
                            k = k + 1
                            exit prune_f
                        end if
                    end do prune_f

                    ! reverse edge
                    tmp = edge(1, :)
                    edge(1, :) = edge(2, :)
                    edge(2, :) = tmp

                    ! it's tradeoff between size and speed, but I prefer size
                    prune_r: do j = 1, a
                        if (all(buf(j, :, :) == edge)) then
                            exit prune_r
                        end if
                        if (j == k) then
                            buf(k, :, :) = edge(:, :)
                            k = k + 1
                            exit prune_r
                        end if
                    end do prune_r
                end if
            end do
        end do

        ! now create a new array of this size
        allocate(edges(k, 2, 2))
        do i = 1, k
            edges(i, :, :) = buf(i, :, :)
            call f_print_edge(buf(i, :, :))
        end do
    end function make_maze
end module maze
program main
    use, intrinsic :: iso_fortran_env, only : int16, int32, real32
    use :: maze
    implicit none
    integer :: edges(120, 2, 2)
    edges = make_maze(5, 6)
    !do i = 1, size(edges, 1)
    !    if (.not. any(edges(i, :, :) == 0)) then
    !        call f_print(edges(i, :, :))
    !    end if
    !end do
end program main
