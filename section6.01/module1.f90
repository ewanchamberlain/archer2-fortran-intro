module module1
  use iso_fortran_env
  implicit none
  integer, parameter :: kp = kind(real64)
contains
  function trapz(a, b, n, f) result(r)
    real (kp), intent(in) :: a, b
    integer, intent(in) :: n
    interface
      function f(x) result(y)
        real, intent(in) :: x
        real :: y
      end function f
    end interface
    real (kp) :: r
    real (kp) :: h 
    integer :: i
    real (kp) :: s
    
    h = (b-a)/n

    s = 0.0_kp
    do i = 1, n-1
      s = s + 2.0_kp * f(a+i*h)
    end do

    r = h*(f(a) + s + f(b)) / 2.0_kp


  end function trapz
end module module1