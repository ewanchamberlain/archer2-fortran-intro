program program1
  use module1
  implicit none

  real (kp) :: pi = 4.0_kp * atan(1.0_kp)
  real (kp) :: a, b
  integer :: n

  a = 0
  b = pi/2.0_kp

  n = 10
  print *, n, trapz(a, b, n, cossin)
  n = 100
  print *, n, trapz(a, b, n, cossin)
  n = 1000
  print *, n, trapz(a, b, n, cossin)

  
contains
  function cossin(x) result(y)
    real (kp), intent(in) :: x
    real (kp) :: y

    y = cos(x) * sin(x)

  end function cossin


end program program1