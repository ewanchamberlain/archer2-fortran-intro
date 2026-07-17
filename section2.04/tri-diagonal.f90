program exercise

  ! Tri-diagonal matrix problem via Thomas' algorithm
  ! See https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm

  ! We will write a program to solve the tri-diagonal matrix problem
  !   a_i x_i-1 + b_i x_i + c_i x_i+1 = d_i
  ! Follow the description at the web page above.

  ! Consider a small problem with n = 4
  ! The following values can be used as a test:
  !   a =      [1.0, 1.0, 1.0]
  !   b = [4.0, 4.0, 4.0, 4.0]
  !   c = [2.0, 2.0, 2.0]
  !   d = [1.0, 4.0, 5.0, 6.0]
  ! which should give a solution approx. x = [-0.195, 0.890, 0.317, 1.42]

  ! 1. Implement the algorithm following the pseudocode.
  ! 2. Check the answer
  ! 3. Construct the full matrix and use the intrinsic function
  !    matmul(a, x) to check the answer for a different d. The
  !    arguments a and x may be an appropriate combination of
  !    matrix and vector.
  ! 4. The above reference states that the algorithm is stable if
  !    the matrix is diagonally dominant. Check this is the case
  !    before entering the solution phase.
  !    See also https://en.wikipedia.org/wiki/Diagonally_dominant_matrix

  use iso_fortran_env, only : real64
  implicit none
  integer, parameter :: kp = real64

  ! For Thomas' method
  real (kp), dimension(2:4) :: a = [1._kp, 1._kp, 1._kp]
  real (kp), dimension(4) :: b = [4._kp, 4._kp, 4._kp, 4._kp]
  real (kp), dimension(3) :: c = [2._kp, 2._kp, 2._kp]
  ! real (kp), dimension(4) :: d = [1._kp, 4._kp, 5._kp, 6._kp]
  real (kp), dimension(4) :: d = [-1._kp, 2.5_kp, 32._kp, -10000._kp]
  integer :: n = size(d)
  real (kp), dimension(:), allocatable :: cp
  real (kp), dimension(:), allocatable :: dp 
  real (kp), dimension(:), allocatable :: x
  integer :: i

  ! For matmul
  real (kp), dimension(:, :), allocatable :: m
  real (kp), dimension(:), allocatable :: d2

  logical :: is_diagonally_dominant = .true.

  ! Check whether the matrix is diagonally dominant
  allocate(m(n, n))
  do i=1, n
    if (i > 1) m(i, i-1) = a(i)
    m(i, i) = b(i)
    if (i < n) m(i, i+1) = c(i)
  end do

  do i=1, n
    if (abs(m(i, i)) < sum(abs(m(i, :i-1))) + sum(abs(m(i, i+1:)))) then
      is_diagonally_dominant = .false.
    exit
    end if
  end do

  if (.not. is_diagonally_dominant) then
    print *, "Matrix is not diagonally dominant!"
  else
    allocate(cp(n-1))
    allocate(dp(n))
    allocate(x(n))

    ! Forward sweep
    cp(1) = c(1) / b(1)
    do i=2, n-1
      cp(i) = c(i) / (b(i) - a(i) * cp(i-1)) 
    end do

    dp(1) = d(1) / b(1)
    do i=2, n
      dp(i) = (d(i) - a(i)*dp(i-1)) / (b(i) - a(i)*cp(i-1))
    end do

    ! Back substitution
    x(n) = dp(n)
    do i=n-1, 1, -1
      x(i) = dp(i) - cp(i)*x(i+1)
    end do


    print *, " x: ", x

    allocate(d2(n))
    d2 = matmul(m, x)

    print *, " d: ", d
    print *, "d2: ", d2
  end if
end program exercise
