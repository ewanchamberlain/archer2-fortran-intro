module large_mat_mod
  use iso_fortran_env
  implicit none

  integer, parameter :: r64 = kind(real64)

  type :: sparse_mat
    integer :: nrows, ncols, nnonzero
    integer, dimension(:, :), allocatable :: coords
    integer, dimension(:), allocatable :: elmnts
  contains
    procedure :: read_mat
    procedure :: sparse_to_dense
  end type sparse_mat

contains
  subroutine read_mat(self, filename, ierr)
    class (sparse_mat), intent(inout) :: self
    character (len=*), intent(inout) :: filename
    integer, intent(out) :: ierr
    character (len=256) :: iomsg
    character (len=1024) :: line
    integer :: unit
    integer :: i = 0
    
    open(newunit=unit, file=filename, form="formatted", access="stream", &
          action="read", status="old")
    do
      read (unit=unit, fmt="(a)", err=999, iomsg=iomsg, iostat=ierr) line 
      if (ierr == iostat_end) exit
      i = i + 1
      if (i == 1) then
        continue
      else if (i == 2) then
        block
          integer :: nrows, ncols, nnonzero
          read (line, *) nrows, ncols, nnonzero
          print *, nrows, ncols, nnonzero
          self%nrows = nrows
          self%ncols = ncols
          self%nnonzero = nnonzero

          allocate(self%coords(2, nnonzero))
          allocate(self%elmnts(nnonzero))
        end block
      else
        block
          integer :: icoord, jcoord
          real (r64) :: val

          read (line, *) icoord, jcoord, val
          self%coords(1, i-2) = icoord
          self%coords(2, i-2) = jcoord
          self%elmnts(i-2) = int(val)
        end block
      end if
    end do

    close (unit=unit, status="keep")
    ierr = 0
    return 

  999 continue
    ierr = -1
    print *, "Error reading file: ", trim(iomsg)
  end subroutine read_mat

  function sparse_to_dense(self) result(dmat)
    class (sparse_mat), intent(in) :: self
    integer, dimension(:, :), allocatable :: dmat
    integer :: i, icoord, jcoord
    real (r64) :: elmnt

    allocate(dmat(self%nrows, self%ncols))

    do i=1, self%nnonzero
      icoord = self%coords(1, i)
      jcoord = self%coords(2, i)
      elmnt = self%elmnts(i)

      dmat(icoord, jcoord) = elmnt
    end do

  end function sparse_to_dense
end module large_mat_mod