program exercise4

  ! Compute an approximation to the conductance of a narrow channel.

  ! The steady volume flux (volume flow rate) Q in a rectangular
  ! capillary of cross section width x height 2b x 2c (with 2b > 2c)
  ! we can write:
  !
  !    Q = -C (dp/dx) / eta
  !
  ! with dp/dx the pressure gradient and eta the dynamic viscosity.
  ! One can define a viscosity-independent conductance C
  !
  !    C = (4/3) b c^3 [ 1 - 6(c/b) \sum_k tanh (a_k b/c)/a_k^5 ]
  !
  ! where a_k = (2k - 1) pi/2 and the sum is k = 1, ..., \inf.
  !
  ! E.g. T. Papanastasiou, G. Georiou, and A. Alexandrou,
  ! "Viscous Fluid Flow" CRC Press, Boca Raton, Florida (2000).

  !  Exercise
  !  Compute the first term in the series (i.e., k = 1):
  !
  !  C_1 = (4/3) b c^3 [ 1 - 6(c/b) tanh(a_k b/c) / a_k^5 ]
  !
  !  We will use the sample values w = 62, h = 30.
  !
  !  You will need the intrinsic function tanh() for hyperbolic tangent.
  !
  !  Your result should be in the region of 97277.88 ( the exact figure
  !  depending on precision).

  !  Try for both real32 and real64.
  !  Does your compiler support kind real128?


  ! Some appropriate output might be ...
  ! print *, "Value of w:       ", w
  ! print *, "Value of h:       ", h
  ! print *, "Value of pi:      ", pi
  ! print *, "Approximation is: ", conductance

  use iso_fortran_env
  implicit none

  real (real32) :: w32 = 62._real32
  real (real32) :: h32 = 30._real32
  real (real32) :: pi32 = 4._real32 * atan(1._real32)
  real (real32) :: b32
  real (real32) :: c32
  real (real32) :: a_1_32
  real (real32) :: c_1_32
  real (real64) :: w64 = 62._real64
  real (real64) :: h64 = 30._real64
  real (real64) :: pi64 = 4._real64 * atan(1._real64)
  real (real64) :: b64
  real (real64) :: c64
  real (real64) :: a_1_64
  real (real64) :: c_1_64
  real (real128) :: w128 = 62._real128
  real (real128) :: h128 = 30._real128
  real (real128) :: pi128 = 4._real128 * atan(1._real128)
  real (real128) :: b128
  real (real128) :: c128
  real (real128) :: a_1_128
  real (real128) :: c_1_128
  integer :: k = 1

  b32 = w32 / 2._real32
  c32 = h32 / 2._real32
  a_1_32 = (2._real32 * k - 1._real32) * pi32 / 2._real32
  c_1_32 = 4._real32 / 3._real32 * b32 * c32**3 * ( 1._real32 - 6._real32*(c32/b32) * tanh(a_1_32 * b32/c32) / a_1_32**5)

  b64 = w64 / 2._real64
  c64 = h64 / 2._real64
  a_1_64 = (2._real64 * k - 1._real64) * pi64 / 2._real64
  c_1_64 = 4._real64 / 3._real64 * b64 * c64**3 * ( 1._real64 - 6._real64*(c64/b64) * tanh(a_1_64 * b64/c64) / a_1_64**5)

  b128 = w128 / 2._real128
  c128 = h128 / 2._real128
  a_1_128 = (2._real128 * k - 1._real128) * pi128 / 2._real128
  c_1_128 = 4._real128 / 3._real128 * b128 * c128**3 * ( 1._real128 - 6._real128*(c128/b128) * tanh(a_1_128 * b128/c128) / a_1_128**5)

  print *, "Real32:"
  print *, "   w = ", w32
  print *, "   h = ", h32
  print *, "  pi = ", pi32
  print *, "  C1 = ", c_1_32

  print *, "Real64:"
  print *, "   w = ", w64
  print *, "   h = ", h64
  print *, "  pi = ", pi64
  print *, "  C1 = ", c_1_64

  print *, "Real128:"
  print *, "   w = ", w128
  print *, "   h = ", h128
  print *, "  pi = ", pi128
  print *, "  C1 = ", c_1_128

end program exercise4
