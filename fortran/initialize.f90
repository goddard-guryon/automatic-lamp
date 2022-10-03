module initialize
    implicit none
    private

    ! get_displacement has to be public for f2py
    public :: get_displacement, initial_pos, initial_vel, particle_shower
contains
    subroutine get_displacement(old, new, dist)
        use, intrinsic :: iso_fortran_env, only: real32
        implicit none
        integer, parameter :: r8 = kind(real32)  ! need this for f2py
        real(kind=r8), intent(inout) :: dist
        real(kind=r8), intent(in) :: old(:), new(:)

        dist = sqrt((old(1) - new(1))**2 + (old(2) - new(2))**2)
    end subroutine get_displacement

    function initial_pos(n, r) result(pos)
        use, intrinsic :: iso_fortran_env, only : int16, real32
        use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
        implicit none
        integer, parameter :: i8 = kind(int16), r8 = kind(real32)
        integer(kind=i8), intent(in) :: n
        real(kind=r8), intent(in) :: r
        real(kind=r8) :: pos(n, 2), disk(2), dist(n)
        integer :: i, j, k = 0

        ! it is possible that our initial disk location may
        ! make it impossible to place n disks, so we check that too
        guard: do while (.true.)

            ! initial disk
            call random_number(disk)

            ! manually shift from range 0<->1 to r<->1-r
            disk = (disk+r)/(1+2.0*r)
            pos(1, :) = disk

            ! other disks
            placer: do i = 2, n
                check: do while (.true.)
 
                    ! place new disk
                    call random_number(disk)
                    disk = (disk+r)/(1+2.0*r)

                    ! check its distance from all other disks
                    do j = 1, n

                        ! using it in multiple procedures, so...
                        call get_displacement(pos(j, :), disk, dist(j))
                    end do

                    ! if the smallest distance is larger than diameter of disks,
                    ! save the new disk location
                    if (minval(dist) > 2.0*r) then
                        pos(i, :) = disk
                        exit check
                    end if

                    ! tried enough times, restart
                    k = k + 1
                    if (k > 5000) exit placer
                end do check
            end do placer

            ! if we've filled the box, just exit
            do i = 1, n

                !check if any disk is uninitialized
                if (pos(i, 1) == 0.0 .or. pos(n, 2) == 0.0) exit

                ! check if all disks are valid
                if (ieee_is_nan(pos(i, 1)) .and. ieee_is_nan(pos(i, 2))) exit

                ! if all disks are fine, exit
                if (i == n) exit guard
            end do
        end do guard
    end function initial_pos

    function initial_vel(n, t) result(vel)
        use, intrinsic :: iso_fortran_env, only : int16, real32
        implicit none
        integer, parameter :: i8 = kind(int16), r8 = kind(real32)
        integer(kind=i8), intent(in) :: n
        real(kind=r8), intent(in) :: t
        integer :: i
        real(kind=r8) :: vel(n, 2), phi, upsilon, v, x, y, sigma, theta
        real(kind=r8), parameter :: k_B = 1.3806503e-23

        ! variance of Maxwell distribution (mass is assumed to be 1)
        sigma = sqrt(k_B * t)

        do i = 1, n

            ! Fortran only has uniform distribution, so we convert
            ! first from uniform to Gaussian via Box-Muller method
            call random_number(phi)
            call random_number(upsilon)
            call random_number(theta)

            ! velocity vectors follow Gaussian distribution
            v = sqrt(-2.0*log(upsilon)) * cos(2.0*acos(-1.0)*phi)

            ! convert from velocity to speed by...hehe...
            ! by picking an angle at random and generating
            ! velocity components from it
            x = v*cos(theta*2.0*acos(-1.0))
            y = v*sin(theta*2.0*acos(-1.0))

            ! and then convert from Gaussian to Maxwell, which is simply
            ! a scaling of variance (and proportionality constant)
            x = x*sigma/sqrt(2.0*acos(-1.0)*k_B)
            y = y*sigma/sqrt(2.0*acos(-1.0)*k_B)

            vel(i, 1) = x*t
            vel(i, 2) = y*t
        end do
    end function initial_vel

    function particle_shower(dat, n, r, t, e_p, orig_n) result(new_dat)
        use, intrinsic :: iso_fortran_env, only : int16, real32, output_unit
        implicit none 
        integer, parameter :: i8 = kind(int16), r8 = kind(real32)
        integer(kind=i8), intent(in) :: orig_n, e_p
        integer(kind=i8) :: n, extras, to_push(n)
        real(kind=r8), intent(in) :: dat(2, n, 2), r, t
        real(kind=r8) :: pos(n, 2), vel(n, 2), p(2), v(1, 2)
        real(kind=r8) :: new_pos(n+1, 2), new_vel(n+1, 2), new_dat(2, n+1, 2)
        real(kind=r8) :: rnd, dist(n)
        integer :: i, j, k = 1

        ! just for easier working
        pos = dat(1, :, :)
        vel = dat(2, :, :)

        ! for safety
        new_pos = 0
        new_vel = 0
        new_dat = 0

        ! check probability for pushing existing particle
        ! instead of introducing a new one
        extras = n - orig_n
        call random_number(rnd)
        if (rnd < (orig_n - extras)/(orig_n + extras)) rnd = 1.0

        ! check which particles are in the entry box
        do i = 1, size(pos, 1)
            if (floor(pos(i, 2)) == e_p) then
                to_push(i) = 1
                k = k + 1
            end if
        end do

        ! now, add a new particle based on probability
        if (rnd > 0.5) then

            ! first, copy existing values
            do i = 1, size(pos, 1)
                new_pos(i, :) = pos(i, :)
                new_vel(i, :) = vel(i, :)
            end do

            ! now find position for new particle
            find_new: do while (.true.)
                call random_number(p)
                p = (p+r)/(1+2.0*r)
                p(2) = p(2) + e_p
                do j = 1, n
                    if (to_push(j) == 1) then
                        call get_displacement(pos(j, :), p, dist(j))
                    end if
                end do

                if (minval(dist) > 2.0*r) then
                    new_pos(n+1, :) = p(:)
                    exit find_new
                end if
            end do find_new

            ! and new velocity (thankfully, we can simply
            ! call initial_vel procedure for this)
            v = initial_vel(1_i8, t)

            ! new particle should have almost no velocity in x-direction
            call random_number(rnd)

            ! so scale the random value between 0 and 1 to between -0.1 and 0.1
            v(1, 1) = rnd*0.2 - 0.1
            new_vel(n+1, :) = v(1, :)

            new_dat(1, :, :) = new_pos(:, :)
            new_dat(2, :, :) = new_vel(:, :)

        ! otherwise, just push an existing particle
        else
            write(unit=output_unit, fmt=*) "pushing another"

            ! choose a particle
            call random_number(rnd)
            k = floor(k+1 * rnd)

            ! now push it downwards
            call random_number(rnd)
            vel(k, 2) = vel(k, 2) - rnd

            new_dat(1, :, :) = pos(:, :)
            new_dat(2, :, :) = vel(:, :)
        end if
    end function particle_shower
end module initialize
