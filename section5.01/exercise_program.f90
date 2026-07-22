program exercise_program

  ! Tri-diagonal matrix problem via Thomas' algorithm
  ! See https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm

  ! The following values are used as a test:
  !   a =      [1.0, 1.0, 1.0]                - lower diagonal
  !   b = [4.0, 4.0, 4.0, 4.0]                - diagonal
  !   c = [2.0, 2.0, 2.0]                     - upper diagonal
  !   d = [1.0, 4.0, 5.0, 6.0]                - rhs
  ! which should give a solution approx. x = [-0.195, 0.890, 0.317, 1.42]


  use exercise_module
  implicit none

  integer, parameter :: nmax = 4

  real (mykind), dimension(nmax)      :: b = [4.0, 4.0, 4.0, 4.0]
  real (mykind), dimension(2:nmax)    :: a = [1.0, 1.0, 1.0]
  real (mykind), dimension(1:nmax-1)  :: c = [2.0, 2.0, 2.0]
  real (mykind), dimension(nmax)      :: d = [1.0, 4.0, 5.0, 6.0]
  real (mykind), dimension(nmax)      :: x

  type (tridiag_matrix) :: m
  type (tridiag_matrix) :: simp_m
  m = create_matrix(b, a, c)

  call tridiagonal_solve(m, d, x)

  print *, "Solution ", x(:)

  simp_m = create_simple_matrix(2.43, 1.2, nmax)
  call tridiagonal_solve(simp_m, d, x)

  print *, "Solution ", x(:)

  call destroy_matrix(m)
  call destroy_matrix(simp_m)

end program exercise_program
