program main
  !use ChapterEight, only: say_hello
  use mod_field, only: Field
  implicit none
  integer, parameter :: im =101, jm=101
  type(Field) :: firstField=Field("Water height", [im,jm])
  !call say_hello()
  print*, "this is tsunami.exe"

  print*, "Initialized field: "//trim(firstField%name)
  print*, "size:", firstField%dims
end program main
