module ChapterSeven
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, ChapterSeven!"
  end subroutine say_hello
end module ChapterSeven
