module initialize
    implicit none
    private

    public :: initial_pos, initial_vel, particle_shower
contains
    subroutine get_displacement(pos, disk, dist, d)
        use, intrinsic :: iso_fortran_env, only: real32
        implicit none
        real(kind=real32) :: pos(2), disk(2), dist, d, dx, dy

        ! negative distance gives NaN, so be extra careful
        dx = pos(1)**2 - disk(1)**2
        dy = pos(2)**2 - disk(2)**2
        if (dx < 0) dx = -dx
        if (dy < 0) dy = -dy
        d = sqrt(dx + dy)
        if (d < dist) then
            dist = d
        end if
    end subroutine get_displacement

    function initial_pos(n, r) result(pos)
        use, intrinsic :: iso_fortran_env, only : int16, real32
        implicit none
        integer(kind=int16), intent(in) :: n
        real(kind=real32), intent(in) :: r
        real(kind=real32) :: pos(n, 2), disk(2), dist = 1.0, d
        integer :: i, j

        ! initial disk
        call random_number(disk)
        pos(1, :) = disk

        ! other disks
        do i = 2, n
            check: do while (.true.)
 
                ! place new disk
                call random_number(disk)

                ! check its distance from all other disks
                do j = 1, i

                    ! using it in multiple procedures, so...
                    call get_displacement(pos(j, :), disk, dist, d)
                end do

                ! if the smallest distance is larger than diameter of disks,
                ! save the new disk location
                if (d > 2.0*r) then
                    pos(i, :) = disk
                    exit check
                end if
            end do check
        end do
    end function initial_pos

    function initial_vel(n, t) result(vel)
        use, intrinsic :: iso_fortran_env, only : int16, real32
        implicit none
        integer(kind=int16), intent(in) :: n
        real(kind=real32), intent(in) :: t
        integer :: i
        real(kind=real32) :: vel(n, 2), phi, upsilon, v, x, y, sigma, theta
        real(kind=real32), parameter :: k_B = 1.3806503e-23

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

            vel(i, 1) = x
            vel(i, 2) = y
        end do
    end function initial_vel

    function particle_shower(dat, n, r, t, e_p, orig_n) result(new_dat)
        use, intrinsic :: iso_fortran_env, only : int16, real32
        implicit none
        integer(kind=int16), intent(in) :: orig_n, e_p
        integer(kind=int16) :: n, extras
        real(kind=real32), intent(in) :: dat(2, n, 2), r, t
        real(kind=real32) :: pos(n, 2), vel(n, 2), p(2), v(1, 2)
        real(kind=real32), allocatable :: new_pos(:, :), new_vel(:, :), new_dat(:, :, :)
        real(kind=real32) :: to_push(n, 2), rnd, dist=1.0, d
        integer :: i, j, k = 1

        ! just for easier working
        pos = dat(1, :, :)
        vel = dat(2, :, :)

        ! check probability for pushing existing particle
        ! instead of introducing a new one
        extras = n - orig_n
        call random_number(rnd)
        if (rnd < (orig_n - extras)/(orig_n + extras)) rnd = 1.0

        ! check which particles are in the entry box
        do i = 1, size(pos, 1)
            if (floor(pos(i, 2)) == e_p) then
                to_push(k, :) = pos(i, :)
                k = k + 1
            end if
        end do

        ! now, add a new particle based on probability
        if (rnd > 0.5) then

            ! first, copy existing values
            allocate(new_pos(n+1, 2))
            allocate(new_vel(n+1, 2))
            allocate(new_dat(2, n+1, 2))
            do i = 1, size(pos, 1)
                new_pos(i, :) = pos(i, :)
                new_vel(i, :) = vel(i, :)
            end do

            ! now find position for new particle
            find_new: do while (.true.)
                call random_number(p)
                p(2) = p(2) + e_p
                do j = 1, size(to_push)
                    call get_displacement(to_push(j, :), p, dist, d)
                end do

                if (d > 2.0*r) then
                    new_pos(n+1, :) = p(:)
                    exit find_new
                end if
            end do find_new

            ! and new velocity (thankfully, we can simply
            ! call initial_vel procedure for this)
            v = initial_vel(1_int16, t)

            ! new particle should have almost no velocity in x-direction
            call random_number(rnd)

            ! so scale the random value between 0 and 1 to between -0.1 and 0.1
            v(1, 1) = rnd*0.2 - 0.1
            new_vel(n+1, :) = v(1, :)

            new_dat(1, :, :) = new_pos(:, :)
            new_dat(2, :, :) = new_vel(:, :)

        ! otherwise, just push an existing particle
        else

            ! choose a particle
            call random_number(rnd)
            k = floor(size(to_push)+1.0 * rnd)

            ! sadly, finding location of this particle in original box
            ! has to be manual (coz all values are reals)
            find_push: do i = 1, size(pos, 1)
                if (pos(i, 1) == to_push(k, 1) .and. pos(i, 2) == to_push(k, 2)) then
                    exit find_push
                end if
            end do find_push

            ! now push it downwards
            call random_number(rnd)
            vel(i, 2) = vel(i, 2) - rnd

            allocate(new_dat(2, n, 2))
            new_dat(1, :, :) = pos(:, :)
            new_dat(2, :, :) = vel(:, :)
        end if
    end function particle_shower
end module initialize
