program main
    use, intrinsic :: iso_fortran_env, only : int16, int32, real32
    use :: maze
    implicit none
    integer :: m = 5, n = 6
    type(node_t), allocatable :: tree(:)
    allocate(tree(m*n))
    tree = make_maze(m, n)
    print *, 8/5+1
end program main
