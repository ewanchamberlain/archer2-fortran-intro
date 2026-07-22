program exercise_program2
  use exercise_module2
  implicit none

  type (my_date) :: date

  date = my_date(1, 12, 1999)

  print *, date
  print "(a,dt)", "Default dt format:      ", date
  print "(a,dt(2,4,5))", "vlist dt format: ", date



end program exercise_program2