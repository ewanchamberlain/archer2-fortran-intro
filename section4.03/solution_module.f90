module solution_module
  use iso_fortran_env
  implicit none
  public
  
contains
  subroutine write_pbm(map, filename, ierr)
    logical, dimension(:, :), intent(in) :: map
    character (len=*), intent(in) :: filename
    integer, intent(out) :: ierr

    integer, dimension(:, :), allocatable :: imap
    integer :: i, j

    integer :: myunit
    integer, dimension(2) :: map_shape
    character (len=256) :: msg

    map_shape = shape(map)
    allocate(imap(map_shape(1), map_shape(2)))

    do i=1, map_shape(1)
      do j=1, map_shape(2)
        if (map(i, j)) imap(i, j) = 1
      end do
    end do

    open(newunit=myunit, file=filename, form="formatted", action="write", &
        status="replace", err=100, iostat=ierr, iomsg=msg)
    write(unit=myunit, fmt="(a)", err=200, iostat=ierr, iomsg=msg) "P1"
    write(unit=myunit, fmt="(i0,1x,i0)", err=200, iostat=ierr, iomsg=msg) map_shape
    write(unit=myunit, fmt="(*(i0,1x))", err=200, iostat=ierr, iomsg=msg) imap
    close(unit=myunit, status="keep", err=300, iostat=ierr, iomsg=msg)

    ierr = 0
    return

  100 continue
    ierr = 1
    print *, "Error opening file: ", trim(msg)
    return

  200 continue
    ierr = 1
    print *, "Error writing to file: ", trim(msg)
    return

  300 continue
    ierr = 1
    print *, "Error closing file: ", trim(msg)

  end subroutine write_pbm

  subroutine write_pgm(map, filename, ierr)
    integer, dimension(:, :), intent(in) :: map
    character (len=*), intent(in) :: filename
    integer, intent(out) :: ierr

    integer :: myunit
    integer, dimension(2) :: map_shape
    character (len=256) :: msg

    map_shape = shape(map)

    open(newunit=myunit, file=filename, form="formatted", action="write", &
        status="replace", err=100, iostat=ierr, iomsg=msg)
    write(unit=myunit, fmt="(a)", err=200, iostat=ierr, iomsg=msg) "P2"
    write(unit=myunit, fmt="(i0,1x,i0)", err=200, iostat=ierr, iomsg=msg) map_shape
    write(unit=myunit, fmt="(*(i0,1x))", err=200, iostat=ierr, iomsg=msg) map
    close(unit=myunit, status="keep", err=300, iostat=ierr, iomsg=msg)

    ierr = 0
    return

  100 continue
    ierr = 1
    print *, "Error opening file: ", trim(msg)
    return

  200 continue
    ierr = 1
    print *, "Error writing to file: ", trim(msg)
    return

  300 continue
    ierr = 1
    print *, "Error closing file: ", trim(msg)

  end subroutine write_pgm

  subroutine write_ppm(map, filename, ierr)
    real (real64), dimension(:, :), intent(in) :: map
    character (len=*), intent(in) :: filename
    integer, intent(out) :: ierr


    integer :: myunit
    integer, dimension(2) :: map_shape
    character (len=256) :: msg

    map_shape = shape(map)

    open(newunit=myunit, file=filename, form="formatted", action="write", &
        status="replace", err=100, iostat=ierr, iomsg=msg)
    write(unit=myunit, fmt="(a)", err=200, iostat=ierr, iomsg=msg) "P3"
    write(unit=myunit, fmt="(i0,1x,i0)", err=200, iostat=ierr, iomsg=msg) map_shape
    write(unit=myunit, fmt="(*(f0.0,1x))", err=200, iostat=ierr, iomsg=msg) map
    close(unit=myunit, status="keep", err=300, iostat=ierr, iomsg=msg)

    ierr = 0
    return

  100 continue
    ierr = 1
    print *, "Error opening file: ", trim(msg)
    return

  200 continue
    ierr = 1
    print *, "Error writing to file: ", trim(msg)
    return

  300 continue
    ierr = 1
    print *, "Error closing file: ", trim(msg)
  end subroutine write_ppm

end module solution_module