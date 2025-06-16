module ChapterSix
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, ChapterSix!"
  end subroutine say_hello
end module ChapterSix
