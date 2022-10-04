!> @brief Maze generation for simulation
!> This module contains functions for creating a maze object
!> The maze path is generated as a tree, and the function
!> returns a list of edges between the nodes of the grid
program maze
    implicit none
    integer :: i
    !private
    type node_t
        integer :: id, n_nbrs = 0
        integer :: nbr_0 = 0, nbr_1 = 0, nbr_2 = 0, nbr_3 = 0
    end type node_t
    !public :: node_t, make_maze
    type(node_t) :: tree(5*6)
    !tree = make_maze(5, 6)
    !do i = 1, size(tree)
    !    associate (t => tree(i))
    !        print '(I0, A, I0, A, I0, A, I0, A, I0, A, I0, A)', t%id, '(', t%n_nbrs, ') => (', &
    !            t%nbr_0, ', ', t%nbr_1, ', ', t%nbr_2, ', ', t%nbr_3, ')'
    !    end associate
    !end do
    !print *, "Done"
contains

    !> @brief get neighboring boxes in the grid
    !> Given a box (=node) location, find its neighboring
    !> boxes in the grid
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

        if (mod(k, m) /= 0) then
            nbrs(i) = k + n
            i = i + 1
        end if
    end function get_nbrs

    !> @brief Add a child node
    !> Small function to connect two nodes to each other
    !> and create a node if only one of the two exists
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
    !> which corresponds to a path through the maze
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

        ! start with a root node
        new_node = node_t(id=1, n_nbrs=1)
        new_node%nbr_0 = 1
        tree(1) = new_node
        current(1) = 1

        ! now propagate
        guard: do while (.true.)

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
                            if (nbrs(j) /= 0 .and. tree(nbrs(j))%n_nbrs == 0) then
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

    function get_edge(x, y, n) result(e)
        implicit none
        integer, intent(in) :: x, y, n
        integer :: e(2, 2, 2)
        if (y == x-1) then
            e = [[[x-1, y-1], [x-1, y]], [[x-1, y], [x-1, y-1]]]
        end if
    end function get_edge

    !> @brief Main function for maze generation
    !> This function generates the maze edges, taking the tree
    !> structure from maze_grid function and converting it into
    !> a list of edges which corresponds to the maze
    !> @param m Height of maze
    !> @param n Width of maze
    !> @return edges List of edges in 
    function make_maze(m, n) result(edges)
        implicit none
        integer, intent(in) :: m, n
        type(node_t) :: tree(m*n)
        integer :: a, i, j, k = 0, buf(4*m*n, 2, 2), edge(2, 2)
        integer, allocatable :: edges(:, :, :)

        tree = maze_grid(m, n)
        k = 1
        do a = 1, size(tree)
        end do
    end function make_maze
end program maze
