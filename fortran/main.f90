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
