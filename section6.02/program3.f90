program program3
  character (len=:), allocatable :: command

  command = "cat README.md"
  call execute_command_line(command)
  
end program program3