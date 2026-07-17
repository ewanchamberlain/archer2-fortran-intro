program game_of_life

  ! Exercise to run the "Game of Life"

  implicit none

  ! Size of the "board". An odd number is chosen so that the initial
  ! pattern has rotational symmetry around the central point in the
  ! square system.

  ! We will use the integers 1 and 0 to represent "live" and "dead",
  ! represetively. A boolean (true/false) value could have be used.

  integer, parameter :: nrow = 21
  integer, parameter :: ncol = 21
  integer, dimension(nrow, ncol) :: board

  character (len=1), dimension(ncol) :: row_state
  integer :: i
  integer :: j
  integer :: neighbour_sum
  integer, dimension(nrow, ncol, 2) :: board2
  integer :: tmax = 5
  integer :: t = 1
  integer :: idx
  integer :: jdx

  ! Initialise the starting state of the board.
  ! The central section of the 21x21 grid looks like this:
  ! .........
  ! ..#.#.#..
  ! ..#...#..
  ! ..#...#..
  ! ..#...#..
  ! ..#.#.#..
  ! .........

  board(:, :)    = 0
  board(9:13, 9) = 1
  board( 9,  11) = 1
  board(13,  11) = 1
  board(9:13,13) = 1


  !  1. Output to screen the initial view of the board.
  print *, "Initial board set-up"
  do i=1, nrow
    row_state = "."
    where (board(i, :) == 1) row_state(:) = "#"
    print *, row_state
  end do

  ! HINT:
  ! The output is printed row by row, so we could create the row of characters
  ! something like this:
  !  a. declare an array of characters to hold one row of the board
  !     (here called "line");
  !  b. for each column, set either "live" or "dead";
  !  c. at the end of each row, print the whole array of characters.

  !  do i = 1, nrow
  !    do j = 1, ncol
  !      if (board(i, j) == 1) then
  !        ! set "live" character "#" for this column
  !      else
  !        ! set "dead" character "." for this column
  !      end if
  !    end do
  !  ! Print current row ..
  !  end do


  ! 2. Compute and output the new state for each position of the board ...
  print *, "First iteration:"
  do i=1, nrow
    row_state = "."
    where (board(i, :) == 1) row_state(:) = "#"
    do j=1, ncol
      if (i==1 .or. i==nrow .or. j==1 .or. j==ncol) cycle
      neighbour_sum = sum(board(i-1, j-1:j+1))
      neighbour_sum = neighbour_sum + board(i, j-1) + board(i, j+1)
      neighbour_sum = neighbour_sum + sum(board(i+1, j-1:j+1))

      if (board(i, j) == 0) then
        if (neighbour_sum == 3) row_state(j) = "#"
      else
        if (.not. (neighbour_sum == 2 .or. neighbour_sum == 3)) row_state(j) = "."
      end if
    end do
    print *, row_state
  end do
  !    HINT
  !    Do not try, at this stage, anything at the perimeter.
  !    Do not try to update the board; just use the character array
  !    to record and output the new state.


  ! 3. Time stepping, run and output the first four or five steps
  board2(:, :, 1)    = 0
  board2(9:13, 9, 1) = 1
  board2( 9,  11, 1) = 1
  board2(13,  11, 1) = 1
  board2(9:13,13, 1) = 1
  board2(:, :, 2) = 0

  print *, "Initial board set-up"
  do i=1, nrow
    row_state = "."
    where (board2(i, :, 1) == 1) row_state(:) = "#"
    print *, row_state
  end do

  do t=1, tmax
    idx = 2 - mod(t, 2)
    jdx = mod(t, 2) + 1
    print *, "t = ", t
    do i=1, nrow
      do j=1, ncol
        if (i==1 .or. i==nrow .or. j==1 .or. j==ncol) cycle

        neighbour_sum = sum(board2(i-1, j-1:j+1, idx))
        neighbour_sum = neighbour_sum + board2(i, j-1, idx) + board2(i, j+1, idx)
        neighbour_sum = neighbour_sum + sum(board2(i+1, j-1:j+1, idx))

        if (board2(i, j, idx) == 0) then
          if (neighbour_sum == 3) then
            board2(i, j, jdx) = 1
          else
            board2(i, j, jdx) = 0
          end if
        else
          if (.not. (neighbour_sum == 2 .or. neighbour_sum == 3)) then
            board2(i, j, jdx) = 0
          else
            board2(i, j, jdx) = 1
          end if
        end if
      end do

      row_state = "."
      where (board2(i, :, jdx) == 1) row_state(:) = "#"
      print *, row_state
    end do
  end do
  !    using an additional outer loop over the time step.
  !    HINT
  !    a. You will need some way to "remember" the previous state
  !       as well as holding the current state. With some thought,
  !       you should be able to do this using a board of dimensions
  !       dimesnion(nrow, ncol, 2), where the third dimension holds
  !       two time levels.
  !    b. It should not be necessary to copy the contents of the board
  !       "between time levels"; just use the time level index.
  !    c. Print the time  at each step to separate the picture of
  !       the time levels.

end program game_of_life
