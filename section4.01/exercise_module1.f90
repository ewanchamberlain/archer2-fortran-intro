module exercise_module1

  ! String functions exercise.
  !
  ! 1. Write a subroutine string_to_lower_case() to replace the argument
  ! 2. Write a function   to_lower_case() to return a new string
  !
  ! See exercise_program1.f90 for a test.

  implicit none
  public

contains
  subroutine string_to_lower_case(string)
    character (len = *), intent(inout) :: string
    integer :: i, s

    do i=0, len(string)
      s = iachar(string(i:i))
      if (s >= 65 .and. s <= 90) then
        string(i:i) = achar(s + 32)
      end if
    end do
  end subroutine string_to_lower_case

  function to_lower_case(string) result(out_string)
    character (len = *), intent(in) :: string
    character (len = :), allocatable :: out_string
    integer :: i, s

    allocate(character(len=len(string)) :: out_string)

    do i=0, len(string)
      s = iachar(string(i:i))
      if (s >= 65 .and. s <= 90) then
        out_string(i:i) = achar(s + 32)
      else
        out_string(i:i) = achar(s)
      end if
    end do
  end function to_lower_case

end module exercise_module1
