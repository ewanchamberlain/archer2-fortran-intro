program solution_program

  ! Establish a logical array an write out a suitable pbm file.

  use solution_module
  use iso_fortran_env
  implicit none

  integer, parameter        :: n = 49
  logical, dimension(n,2*n) :: map

  integer :: i, ierr

  integer, dimension(n, 2*n) :: map2
  real (real64), dimension(n, 2*n) :: map3
  ! Set top row, left column, and the diagonal

  map(:,:) = .false.
  map(1,:) = .true.
  map(:,1) = .true.

  do i = 1, n
     map(i,i) = .true.
  end do

  map2(:, :) = 0
  map2(1, :) = 1
  map2(:, 1) = 100
  do i = 1, n
    map2(i, i) = i*100
  end do

  map3(:, :) = 0.0
  map3(1, :) = 1.0
  map3(:, 1) = 100.0
  do i = 1, n
    map3(i, i) = i*100.0
  end do

  call write_pbm(map, "test49.pbm", ierr)

  print *, "Return value was ", ierr

  call write_pgm(map2, "test49.pgm", ierr)
  print *, "Return value was ", ierr
  call write_ppm(map3, "test49.ppm", ierr)
  print *, "Return value was ", ierr


end program solution_program
