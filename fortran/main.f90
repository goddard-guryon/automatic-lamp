program main
    use, intrinsic :: iso_fortran_env, only : int16, int32, real32
    use :: initialize, only : initial_pos, initial_vel, particle_shower
    implicit none
    integer(kind=int32) :: n = 7_int16
    real(kind=real32) :: r = 0.2, t = 2, pos(7, 2), vel(7, 2), dat(2, 7, 2), new_dat(2, 8, 2)=0
    pos = initial_pos(n, r)
    !vel = initial_vel(n, t)
    !dat(1, :, :) = pos(:, :)
    !dat(2, :, :) = vel(:, :)
    !new_dat = particle_shower(dat, n, r, t, 10, 6)
    print *, pos
end program main
