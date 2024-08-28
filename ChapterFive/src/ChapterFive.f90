module ChapterFive
  use iso_fortran_env, only: real32
  implicit none
  private

  public :: say_hello, readStock, alloc, free, reverse
contains
  subroutine say_hello
    print *, "Hello, ChapterFive!"
  end subroutine say_hello

  subroutine readStock(filename, time, open, high, low, close,adjclose, volume)
    character(*), intent(in):: filename
    character(:),allocatable, intent(inout):: time(:)
    real(real32), allocatable, intent(inout) ::open(:),high(:),low(:),close(:),adjclose(:), volume(:)

    integer :: fileUnit
    integer :: n, nm

    nm = numRecords(filename)-1

    if (allocated(time)) deallocate(time)
    allocate(character(10) :: time(nm))
    call alloc(open, nm)
    call alloc(high, nm)
    call alloc(low, nm)
    call alloc(close, nm)
    call alloc(adjclose, nm)
    call alloc(volume, nm)

    open(newunit = fileUnit, file=filename)
    read(fileUnit, fmt=*, end=1)

    do n = 1, nm
      read(fileUnit, fmt=*, end=1) &
            time(n), open(n), high(n), low(n), close(n), adjclose(n),volume(n)
    end do

    1 close(fileUnit)

  end subroutine readStock

  integer function numRecords(fileName)
    character(len=*), intent(in) :: fileName
    integer :: fileUnit
    open(newunit=fileUnit, file=fileName)
    numRecords = 0
    do
      read(unit=fileUnit, fmt=*, end=1)
      numRecords = numRecords + 1
    end do
    1 continue
    close(unit=fileUnit)
  end function numRecords

  subroutine alloc(a, arrSize)
    real, allocatable, intent(inout) :: a(:)
    integer :: arrSize
    integer:: stat
    character(len=100) :: errmsg

    if (allocated(a)) call free(a)
    allocate(a(arrSize), stat=stat, errmsg=errmsg)
    if(stat > 0) error stop errmsg

  end subroutine alloc

  subroutine free(a)
    real, allocatable, intent(inout):: a(:)
    integer:: stat
    character(len=100):: errmsg

    if(.not. allocated(a)) return
    deallocate(a, stat=stat, errmsg=errmsg)
    if (stat>0) then
      error stop errmsg
    end if

  end subroutine free

  pure function reverse(a)
    real, intent(in):: a(:)
    real :: reverse(size(a))
    reverse = a(size(a):1:-1)
  end function reverse
end module ChapterFive
