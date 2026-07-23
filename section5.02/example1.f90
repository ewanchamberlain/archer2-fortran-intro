program example1

  ! Use of pointer and target

  implicit none

  integer, target          :: datum = 1
  integer, pointer :: p => null()

  print *, associated(p)
  p => datum
  print *, associated(p)
  p = 2

  print *, "datum is ", datum

end program example1
