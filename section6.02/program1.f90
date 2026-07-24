program program1
  implicit none
  integer :: nmax, n, len, stat
  character (len=:), allocatable :: val

  nmax = command_argument_count()
  print *, nmax

  do n=1, nmax
    call get_command_argument(n, length=len, status=stat)
    allocate(character (len=len) :: val)
    call get_command_argument(n, val, len, stat)
    print *, n, trim(val), len, stat
    deallocate(val)
  end do
end program program1