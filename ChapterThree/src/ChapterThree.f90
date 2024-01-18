module ChapterThree
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, ChapterThree!"
  end subroutine say_hello
end module ChapterThree
