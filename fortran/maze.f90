!> @brief Maze generation for simulation
!> This module contains functions for creating a maze object.
!> The maze path is generated as a tree, and the function
!> returns a list of edges between the nodes of the grid.
module maze
    implicit none
    type node_t
        integer :: id, n_nbrs = 0, nbr_0 = 0, nbr_1 = 0, nbr_2 = 0, nbr_3 = 0
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
            child%nbr_0 = parent%id
        else
            child = tree(nbr_id)
            if (child%nbr_1 /= 0) then
                if (child%nbr_2 /= 0) then
                    child%nbr_3 = parent%id
                else
                    child%nbr_2 = parent%id
                end if
            else
                child%nbr_1 = parent%id
            end if
        end if
        if (parent%nbr_1 /= 0) then
            if (parent%nbr_2 /= 0) then
                parent%nbr_3 = child%id
            else
                parent%nbr_2 = child%id
            end if
        else
            parent%nbr_1 = child%id
        end if
    end function add_child

    !> @brief Generation of maze path as a tree
    !> This generates the tree object as an array of nodes
    !> which corresponds to a path through the maze.
    !> @param m The number of rows (height) of maze
    !> @param n The number of columns (width) of maze
    !> @return tree Array of node_t objects
    function maze_grid(m, n) result(tree)
        implicit none
        integer, intent(in) :: m, n
        integer :: current(m*n), i, j, nbrs(4), check
        type(node_t) :: tree(m*n)
        type(node_t), target :: new_node
        real :: rnd

        ! safety loop
        guard: do while (.true.)

            !reinitialize
            current = 0
            do i = 1, size(tree)
                tree(i) = node_t(id=0, n_nbrs=0, nbr_0=0, nbr_1=0, nbr_2=0, nbr_3=0)
            end do

            ! start with a root node
            new_node = node_t(id=1, n_nbrs=1)
            new_node%nbr_0 = 1
            tree(1) = new_node
            current(1) = 1

            ! check if we're done
            check = 0
            do i = 1, size(tree)
                if (tree(i)%n_nbrs == 0) check = check + 1
            end do
            if (check == 0) exit guard

            ! keep propagating until we have nowhere to go
            do while (any(current == 0))

                ! propagate at each block
                do i = 1, size(tree)
                    if (current(i) /= 0) then
                        nbrs = get_nbrs(i, m, n)

                        ! iterate over list of neighbors
                        more: do j = 1, 4

                            ! only connect if target node isn't already connected
                            ! lest there shall be no borders in the world
                            if (nbrs(j) /= 0) then

                                ! for some reason, I can't even use .and. here :)
                                if (tree(nbrs(j))%n_nbrs <= 4) then
                                    new_node = add_child(tree, tree(i), nbrs(j))
                                    tree(nbrs(j)) = new_node
                                    tree(i)%n_nbrs = tree(i)%n_nbrs + 1
                                    current(nbrs(j)) = 1
                                    current(i) = 0
                                    call random_number(rnd)
                                    if (rnd > 0.5) then
                                        exit more
                                    end if
                                end if
                            end if
                        end do more
                    end if
                end do

                ! idk what I'm doing wrong, but the `current`
                ! array isn't being modified properly; hence this
                ! additional check
                check = 0
                do i = 1, size(tree)
                    if (tree(i)%n_nbrs == 0) check = check + 1
                end do
                if (check == 0) exit guard
            end do
        end do guard
    end function maze_grid

    !> @brief Print the list of edges
    !> Simply call this subroutine to print an edge array in a
    !> formatted manner. To print the whole edges object, iterate
    !> over the object and call this subroutine on each item in it.
    !> @param e Edge array (2x2x2 array)
    !> @param extra Extra (optional) string to print after edge
    subroutine f_print(e, extra)
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
    function get_edge(x, y, m, n) result(e)
        implicit none
        integer, intent(in) :: x, y, m, n
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
        integer :: a, i, j, k = 0, l, buf(8*m*n, 2, 2), edge(2, 2), tmp(2)
        integer, allocatable :: edges(:, :, :)

        tree = maze_grid(m, n)
        k = 1
        do a = 1, size(tree)
            i = tree(a)%id

            ! if only Fortran had f-strings
            do l = 1, 3
                select case(l)
                    case (1)
                        j = tree(a)%nbr_1
                    case (2)
                        j = tree(a)%nbr_2
                    case (3)
                        j = tree(a)%nbr_3
                end select
                if (j /= 0) then
                    edge = get_edge(i, j, m, n)
                    if (.not. any(edge < 0)) then
                        buf(k, :, :) = edge(:, :)
                        tmp = edge(1, :)
                        edge(1, :) = edge(2, :)
                        edge(2, :) = tmp
                        buf(k+1, :, :) = edge(:, :)
                        k = k + 2
                    end if
                end if
            end do
        end do

        ! now create a new array of this size
        allocate(edges(k, 2, 2))
        do i = 1, k
            edges(i, :, :) = buf(i, :, :)
            call f_print(buf(i, :, :))
        end do
    end function make_maze
end module maze
