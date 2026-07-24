program program2
  ! character (len=8) :: date
  ! character (len=10) :: time
  character (len=5) :: zone
  integer, dimension(8) :: ivalues

  call date_and_time(zone=zone, values=ivalues)

  ! print *, date
  ! print *, time
  print *, zone
  print *, ivalues

  write(*, fmt="(i2.2'/'i2.2'/'i4)", advance="no") ivalues(3), ivalues(2), ivalues(1)
  write(*, fmt="(' 'i2.2':'i2.2':'i2.2'.'i3.3)", advance="no") ivalues(5:8)
  write(*, fmt="(' UTC ', a, ':', a )") zone(1:3), zone(4:5)
  
end program