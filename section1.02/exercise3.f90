program exercise3

  ! Compute an approximation to pi.

  ! An approximation using the Gauss-Legendre algorithm
  ! See, e.g., https://en.wikipedia.org/wiki/Gauss-Legendre_algorithm
  !
  ! If we initialise:
  !
  !   a_0 = 1
  !   b_0 = 1/sqrt(2)
  !   t_0 = 1/4
  !   p_0 = 1
  !
  ! (in general, a_n etc, with n = 0), then the next terms in the series
  ! can be computed as:
  !
  !   a_{n+1} = (a_n + b_n)/2
  !   b_{n+1} = sqrt(a_n b_n)
  !   t_{n+1} = t_n - p_n (a_n - a_{n+1})^2   with p_{n+1} = 2p_n
  !
  !  then
  !
  !   pi_n \approx (a_n + b_n)^2/4t_n
  !
  !  Compute the first two approximations pi_0 and pi_1.

  implicit none
  real :: a_0 = 1.
  real :: b_0 = 1. / sqrt(2.)
  real :: t_0 = 1. / 4.
  real :: p_0 = 1.
  real :: pi_0
  real :: a_1 
  real :: b_1
  real :: t_1
  real :: pi_1

  pi_0 = (a_0 + b_0)**2 / (4*t_0)

  a_1 = (a_0 + b_0) / 2
  b_1 = sqrt(a_0 * b_0)
  t_1 = t_0 - p_0 * (a_1 - a_0)**2
  pi_1 = (a_1 + b_1)**2 / (4*t_1)

  print *, "pi_0 = ", pi_0
  print *, "pi_1 = ", pi_1
end program exercise3
