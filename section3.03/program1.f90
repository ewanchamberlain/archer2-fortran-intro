program program1

  ! Is there a problem here... if so, where?

  use module1
  implicit none
  integer, parameter :: imax = 3
  integer, parameter :: jmax = 2

  real, dimension(0:imax+1,0:jmax+1) :: a
  real, dimension(1:imax,  1:jmax)   :: b

  print *, lbound(a), ubound(a)
  print *, lbound(b), ubound(b) 

  a(:, :) = 0.0
  a(1:imax,1:jmax) = 1.0
  b(:,:) = 1.0

  ! call array_action2(b, a)
  call array_action2(b, a(1:imax,1:jmax))

  print *, "Values a: ", a(:,:)

end program program1
