module cgradient
  use iso_fortran_env
  implicit none

  integer, parameter :: r64 = kind(real64)
  integer, parameter :: k_real = kind(real64)
contains
  subroutine cg_solve(a, b, tol, x, ierr)
    real (r64), dimension(:, :), intent(in) :: a
    real (r64), dimension(:), intent(in) :: b
    real (r64), intent(in) :: tol
    real (r64), dimension(:), intent(inout) :: x
    integer, intent(out) :: ierr

    integer :: n
    real (r64), dimension(:), allocatable :: x0
    real (r64), dimension(:), allocatable :: x1
    real (r64), dimension(:), allocatable :: r0
    real (r64), dimension(:), allocatable :: r1
    real (r64), dimension(:), allocatable :: p0
    real (r64), dimension(:), allocatable :: p1
    real (r64) :: alpha, beta
    integer, dimension(2) :: ashape

    n = size(x)
    allocate(x0(n))
    allocate(r0(n))
    allocate(x1(n))
    allocate(r1(n))
    allocate(p0(n))
    allocate(p1(n))

    ashape = shape(a)
    if (ashape(1) /= n .or. ashape(2) /= n) then
      ierr = 1
      print "('A wrong shape: expected (', i1, ', ' i1, '), got (', i1, ', ' i1 ').')", n, n, ashape
      return
    end if

    if (size(b) /= n) then
      ierr = 2
      print "('B wrong size: expected ', i1, 'got ', i1, '.')", n, n, size(b)
      return
    end if


    x0(:) = x(:)
    r0 = b - matmul(a, x0)
    p0(:) = r0(:)

    if (sum(r0*r0) <= tol) then
      x(:) = x0(:)
      ierr = 0
      return
    end if

    do
      alpha = sum(r0 * r0) / sum(p0 * matmul(a, p0))
      x1 = x0 + alpha * p0
      r1 = r0 - alpha * matmul(A, p0)

      if (sum(r1*r1) <= tol) exit

      beta = sum(r1 * r1) / sum(r0 * r0)
      p1 = r1 + beta * p0

      x0(:) = x1(:)
      r0(:) = r1(:)
      p0(:) = p1(:)
    end do

    x(:) = x1(:)
    ierr = 0

  end subroutine cg_solve



end module cgradient