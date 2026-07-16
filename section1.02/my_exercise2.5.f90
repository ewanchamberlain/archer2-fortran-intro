program exercise2_5
    implicit none

    complex :: z = (-1., 0.)
    complex :: i
    real :: x = -1.
    real :: j

    i = sqrt(z)
    j = sqrt(x)

    print *, "Sqrt(-1.+0.i) = ", i
    print *, "Sqrt(-1.) = ", j
end program exercise2_5