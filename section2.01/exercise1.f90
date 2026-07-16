program exercise1

  ! Compute an approximation to pi.

  ! An approximation using the Gauss-Legendre algorithm
  ! See, e.g., https://en.wikipedia.org/wiki/Gauss-Legendre_algorithm
  !
  ! Add an iteration which computes successive approximations
  ! to pi (say the first 5 or 6)
  !
  ! Try extended precision kind(1.d0)
  ! Can you add a condition that will exit the iteration when
  ! the approximation is close enough to the true value.
  ! Hint: you will need the intrinsic function abs() which returns
  ! the absolute value of the argument.

  implicit none

  integer, parameter :: kp = kind(1.d0)

  real (kp) :: a = 1.0_kp
  real (kp) :: b = 1.0/sqrt(2.0_kp)
  real (kp) :: t = 0.25_kp
  real (kp) :: p = 1.0_kp
  real (kp) :: pi

  real (kp) :: an

  integer :: nmax = 6
  integer :: i
  real (kp) :: eps = 1d-10
  
  real (kp) :: pi_true = 4._kp * atan(1._kp)

  print *, "Approximation pi_0: ", (a + b)**2/(4.0*t)

  do i=1, nmax
    an = a

    a = (an + b)/2.0
    b = sqrt(an*b)
    t = t - p*(an - a)**2
    p = 2.0*p
    pi = (a + b)**2/(4.0*t)

    print *, "Approximation pi_", i, ": ", pi
    if (abs(pi - pi_true) <= eps) then
      print *, "Converged after ", i, " iterations."
      exit
    end if
  end do

end program exercise1
