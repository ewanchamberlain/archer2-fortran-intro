module exercise_module2
  implicit none
  public

  type, public :: my_date
    integer :: day = 1        ! day 1-31
    integer :: month = 1      ! month 1-12
    integer :: year = 1900    ! year
  contains
    procedure :: my_date_write_formatted
    generic :: write(formatted) => my_date_write_formatted
  end type my_date
contains
  subroutine my_date_write_formatted(self, unit, iotype, vlist, iostat, iomsg)
    class (my_date), intent(in) :: self
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg

    if (iotype == "LISTDIRECTED") then
      write(unit=unit, fmt="(i2.2,'/',i2.2,'/',i4.4)", err=900, iostat=iostat, iomsg=iomsg) self%day, self%month, self%year
    else if (iotype == "DTdesc-string") then
      write(unit=unit, fmt="(a,i1,a,i2,a,i1,a)") &
                "(i", vlist(1), ",a", vlist(2), ",i", vlist(3), ")"
    else
      write(unit=unit, fmt=*, err=900, iostat=iostat, iomsg=iomsg) self%day, self%month, self%year
    end if

    return

  900 continue
    print *, "Error reading file: ", trim(iomsg)
  end subroutine my_date_write_formatted

end module exercise_module2