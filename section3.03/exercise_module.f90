module exercise_module
  use iso_fortran_env
  integer, parameter :: mykind = real32
  public
  contains

  subroutine tridiagonal_solve(b, a, c, rhs, x)
    real (mykind), dimension(1:), intent(in) :: b
    real (mykind), dimension(2:size(b)), intent(in) :: a
    real (mykind), dimension(1:size(b)-1), intent(in) :: c
    real (mykind), dimension(1:size(b)), intent(in) :: rhs
    real (mykind), dimension(1:size(b)), intent(out) :: x
    real (mykind), dimension(1:size(b)) :: local_b
    real (mykind), dimension(1:size(b)) :: local_rhs
    integer :: i
    real (mykind) :: w
    integer :: nmax

    nmax = size(b)
    local_b(:) = b(:)
    local_rhs(:) = rhs(:)

    do i = 2, nmax
      w = a(i) / local_b(i-1)
      local_b(i) = local_b(i) - w*c(i-1)
      local_rhs(i) = local_rhs(i) - w*local_rhs(i-1)
    end do

    x(:) = local_rhs(:)/local_b(:)

    do i = nmax-1, 1, -1
      x(i) = (local_rhs(i) - c(i)*x(i+1))/local_b(i)
    end do
  end subroutine tridiagonal_solve

end module