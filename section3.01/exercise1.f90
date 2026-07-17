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

  use my_module1
  implicit none

  real (r64) :: pi

  pi = get_pi()
  print *, pi

end program exercise1
