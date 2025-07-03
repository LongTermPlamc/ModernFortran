module ChapterEight
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, ChapterEight!"
  end subroutine say_hello
end module ChapterEight
