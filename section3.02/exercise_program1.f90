program exercise_program1

  ! Functions and subroutines

  use exercise_module1
  implicit none

  real (kp) :: pi
  integer :: i
  integer :: nmax = 10
  integer :: fib

  print *, "pi: ", pi_gauss_legendre()
  call pi_gauss_n(2, pi)
  print *, "pi_3: ", pi
  print *, ""

  do i=1, nmax
    fib = fibonacci(i)
    print *, i, ": ", fib
  end do


end program exercise_program1
