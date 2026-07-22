program program1

  ! A linear congruential generator
  ! See, e.g, https://en.wikipedia.org/wiki/Linear_congruential_generator

  ! Some very bad values might be: my_rng(1, 1, 0, 2147483647)

  use module1
  implicit none

  ! type (my_rng) :: rng = my_rng(a=2, s=1, c=0, m=2147483647)
  type(my_rng) :: rng = my_rng()
  rng%a = 1
  integer :: n

  do n = 1, 6
    print *, "Step ", n, my_rng_int(rng)
  end do

end program program1
