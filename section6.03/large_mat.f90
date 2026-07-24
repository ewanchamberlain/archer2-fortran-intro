program large_mat
  use large_mat_mod 
  use pbm_image, only : write_pbm
  implicit none

  type (sparse_mat) :: mat
  character (len=:), allocatable :: filename
  integer :: ierr
  integer, dimension(:, :), allocatable :: dmat
  integer :: i
  filename = "gr_30_30.mtx"
  call mat%read_mat(filename, ierr)
  print *, mat%elmnts

  dmat = mat%sparse_to_dense()

  filename = "gr_30_30.pbm"
  call write_pbm(dmat, filename, ierr)
  
end program large_mat