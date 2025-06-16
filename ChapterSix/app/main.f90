program main
  !!use ChapterSix, only: say_hello
  use iso_fortran_env, only: stdin => input_unit, stdout=>output_unit, stderr=>error_unit
  implicit none

  character(len=1000):: message

  read(stdin,"(a)") message
  write(stdout,"(a)") trim(message)
  write(stderr,"(a)") "This is an error message"

  write(*,*) stdin, stdout, stderr

  
end program main
