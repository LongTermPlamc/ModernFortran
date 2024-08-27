module ChapterFive
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, ChapterFive!"
  end subroutine say_hello
end module ChapterFive
