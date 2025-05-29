program main
  !!use ChapterSix, only: say_hello
  implicit none
  character(len=1000):: message
  read*, message
  print*, trim(message)
  
end program main
