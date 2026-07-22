module exercise_module

  ! Tri-diagonal matrix problem via Thomas' algorithm
  ! See https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm

  use iso_fortran_env
  implicit none

  integer, parameter :: mykind = real32

  type, public :: tridiag_matrix

    real (mykind), dimension(1:), allocatable :: b(:)
    real (mykind), dimension(2:), allocatable :: a(:)
    real (mykind), dimension(1:), allocatable :: c(:)

  end type tridiag_matrix

contains

  function create_matrix(b, a, c) result(m)
    real (mykind), dimension(1:), intent(in) :: b
    real (mykind), dimension(2:), intent(in) :: a
    real (mykind), dimension(1:), intent(in)  :: c
    type (tridiag_matrix) :: m

    m = tridiag_matrix(b, a, c)
  
  end function create_matrix

  function create_simple_matrix(b, a, n) result(m)
    real (mykind), intent(in) :: b, a
    integer, intent(in) :: n
    type (tridiag_matrix) :: m

    real (mykind), dimension(1:n) :: b_arr
    real (mykind), dimension(2:n) :: a_arr
    real (mykind), dimension(1:n-1) :: c_arr

    b_arr(:) = b
    a_arr(:) = a
    c_arr(:) = a

    m = create_matrix(b_arr, a_arr, c_arr)

  end function create_simple_matrix

  subroutine destroy_matrix(m)
    type (tridiag_matrix), intent(inout) :: m

    deallocate(m%a)
    deallocate(m%b)
    deallocate(m%c)

  end subroutine destroy_matrix

  subroutine tridiagonal_solve(m, rhs, x)

    ! Routine to solve system for
    !   b(1:nmax)                  diagonal elements
    !   a(2:nmax)                  lower diagonal elements
    !   c(1:nmax-1)                upper diagonal elements
    !   rhs(1:nmax)                right-hand side
    !   x(1:nmax)                  solution on exit

    type (tridiag_matrix) :: m
    real (mykind), dimension(1:), intent(in)  :: rhs
    real (mykind), dimension(1:), intent(out) :: x

    real (mykind), dimension(size(m%b)) :: blocal       ! local copy of b
    real (mykind), dimension(size(m%b)) :: rlocal       ! local copy of rhs

    integer :: nmax
    integer :: i
    real (mykind) :: w

    nmax = size(m%b)
    blocal(1:nmax) = m%b(1:nmax)
    rlocal(1:nmax) = rhs(1:nmax)

    ! Solve via Thomas' algorithm

    do i = 2, nmax
      w = m%a(i) / blocal(i-1)
      blocal(i) = blocal(i) - w*m%c(i-1)
      rlocal(i) = rlocal(i) - w*rlocal(i-1)
    end do

    x(:) = rlocal(:)/blocal(:)

    do i = nmax-1, 1, -1
      x(i) = (rlocal(i) - m%c(i)*x(i+1))/blocal(i)
    end do

  end subroutine tridiagonal_solve

end module exercise_module
