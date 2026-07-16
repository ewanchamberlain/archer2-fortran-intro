program exercise1

  ! Exercise to introduce array storage

  ! Here's a version of the pi program
  ! See, e.g., https://en.wikipedia.org/wiki/Pi
  !
  ! If we initialise:
  !
  !   a_0 = 1, b_0 = 1/sqrt(2), t_0 = 1/4, p_0 = 1
  !
  ! and perform the iteration
  !
  !   a_{n+1} = (a_n + b-n)/2
  !   b_{n+1} = sqrt(a_n b_n)
  !   t_{n+1} = t_n - p_n (a_n - a_{n+1})^2   with p_{n+1} = 2p_n
  !
  !  then
  !
  !   pi_n \approx (a_n + b_n)^2/4t_n
  !
  ! Exercise:
  ! Decide on a fixed number of iterations, and introduce arrays of
  ! appropriate size for the quantities a, b, and t. Use a first
  ! loop to assign values to these three quantities.


  implicit none

  integer, parameter :: kp = kind(1.d0)

  real (kp) :: a = 1.0_kp
  real (kp) :: b = 1.0/sqrt(2.0_kp)
  real (kp) :: t = 0.25_kp
  real (kp) :: p = 1.0_kp

  real (kp) :: an
  integer   :: n = 0
  
  integer, parameter :: nmax = 6
  real (kp), dimension(:), allocatable :: a_arr
  real (kp), dimension(:), allocatable :: b_arr
  real (kp), dimension(:), allocatable :: t_arr
  real (kp) :: eps = 1d-10
  real (kp) :: pi_true = 4.0_kp * atan(1.0_kp)
  real (kp) :: pi
  integer :: i = 0

  do
    pi = (a + b)**2/(4.0*t)
    print *, "Approximation n, pi: ", n, pi
    if (abs(pi - pi_true) <= eps) exit

    an = a

    a = (an + b)/2.0
    b = sqrt(an*b)
    t = t - p*(an - a)**2
    p = 2.0*p

    n = n + 1
  end do
  allocate(a_arr(n+1))
  allocate(b_arr(n+1))
  allocate(t_arr(n+1))

  a = 1.0_kp
  b = 1.0/sqrt(2.0_kp)
  t = 0.25_kp
  p = 1.0_kp
  a_arr(1) = a
  b_arr(1) = b
  t_arr(1) = t
  do i=1, n
    pi = (a + b)**2/(4.0*t)
    print *, "Approximation n, pi: ", i, pi

    an = a

    a = (an + b)/2.0
    b = sqrt(an*b)
    t = t - p*(an - a)**2
    p = 2.0*p

    a_arr(i+1) = a
    b_arr(i+1) = b
    t_arr(i+1) = t

  end do

  print *, "a: ", a_arr
  print *, "b: ", b_arr
  print *, "t: ", t_arr

  do i=1, n
    print *, "pi_", i, "=", (a_arr(i) + b_arr(i))**2 / (4.0*t_arr(i))
  end do
end program exercise1
