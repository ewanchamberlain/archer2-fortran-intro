program exercise2

  ! A program to identify prime numbers (<= nmax = 120) via th Sieve
  ! of Eratosthenses.

  ! See, e.g., https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
  !
  ! Define a logical array to indicate where relevant integers are
  ! prime.
  !
  ! Implement the sieve to set appropriate values in the logical array
  ! using the algorithm described at the reference above.
  !
  ! Try using loops first; where could you then introduce array
  ! constructs?
  ! Hint 2. You may need an additional integer array to use array constructs.
  !
  ! Count how many prime numbers you have. Check your results.
  !
  ! Is one version any clearer than the other?

  implicit none

  integer, parameter :: nmax = 120

  logical, dimension(2:nmax - 1) :: is_prime
  ! integer :: i
  integer :: p = 2
  integer :: num_p = 0

  is_prime(:) = .true.
  
  do p=2, nmax
    if (.not. is_prime(p)) cycle
    num_p = num_p + 1
    print *, "Prime no ", num_p, ": ", p
    is_prime(2*p:nmax:p) = .false.
    ! do i=p+1, nmax
    !   if (mod(i, p) == 0) then
    !     is_prime(i) = .false.
    !   end if
    ! end do
end do
end program exercise2
