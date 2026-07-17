module my_module1
  use iso_fortran_env, only : real64
  implicit none
  private

  integer, parameter, public :: r64 = real64
  public :: get_pi

  contains
    function get_pi() result(pi)
      real (r64) :: a = 1.0_r64
      real (r64) :: b = 1.0/sqrt(2.0_r64)
      real (r64) :: t = 0.25_r64
      real (r64) :: p = 1.0_r64
      real (r64) :: an
      integer :: i
      integer :: n = 5
      real (r64) :: pi

      do i=1, n
        an = a
        a = (an + b) / 2.0
        b = sqrt(an * b)
        t = t - p * (an - a)**2
        p = 2.0 * p
      end do

      pi = (a + b)**2 / (4*t)
    end function get_pi
end module