module exercise_module1

  ! Functions and subroutines
  !
  ! 1. Check the existing pi_gauss_legendre() can be declared pure
  ! 2. Add a subroutine pi_gauss_legendre(nmax, pi)
  ! 3. Add a recursive function fibonacci(n) to return
  !    nth fibonacci number for n >= 1, with f_1 = 1, f_2 = 1, ...
  !    n <= 0 might be treated as erroneous

  implicit none
  public

  integer, parameter :: kp = kind(1.d0)

contains

  pure function pi_gauss_legendre() result(pi)

    integer, parameter :: nmax = 10

    real (kp) :: pi

    real (kp) :: a, an, b, t, p
    integer   :: n

    ! Initial values, and iterate ...
    a = 1.0_kp
    b = 1.0/sqrt(2.0_kp)
    t = 0.25_kp
    p = 1.0_kp

    do n = 1, nmax
      an = a
      a = (an + b)/2.0
      b = sqrt(an*b)
      t = t - p*(an - a)**2
      p = 2.0*p
    end do

    pi = (a + b)**2/(4.0*t)

  end function pi_gauss_legendre

  subroutine pi_gauss_n(n, pi)
    integer, intent(in) :: n
    real (kp), intent(out) :: pi
    real (kp) :: a, an, b, t, p
    integer   :: i

    ! Initial values, and iterate ...
    a = 1.0_kp
    b = 1.0/sqrt(2.0_kp)
    t = 0.25_kp
    p = 1.0_kp

    do i = 1, n
      an = a
      a = (an + b)/2.0
      b = sqrt(an*b)
      t = t - p*(an - a)**2
      p = 2.0*p
    end do

    pi = (a + b)**2/(4.0*t)
  end subroutine pi_gauss_n

  recursive function fibonacci(n) result(nf)
    ! ... implementation...
    integer, intent(in) :: n
    integer :: nf

    if (n == 0) then
      nf = 0
    else if (n == 1) then
      nf = 1
    else
      nf = fibonacci(n-1) + fibonacci(n-2)
    end if
  end function fibonacci


  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------

end module exercise_module1
