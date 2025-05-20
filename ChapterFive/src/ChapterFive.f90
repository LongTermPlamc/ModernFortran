module ChapterFive
  
  implicit none
  private

  public :: readStock, alloc, myfree, num_records, reverse!!, mean, std, movingAverage, movingStd, writeStock
contains
  !! This function just parses the file until EOF and records how many lines are there.
  integer function num_records(filename) 
  character(len=*), intent(in):: filename
  integer :: fileunit

  open(newunit=fileunit, file=filename)
  num_records = 0
  do
    read(unit=fileunit, fmt=*, end=1) !! This end = 1 enable the program to jump to line labeled with 1 when EOF is reached.
    num_records = num_records+1
  end do
  1 continue
  close(unit=fileunit)

  end function num_records

  !! This function reads the file "filename" and stores each column in a diferent array. 
  subroutine readStock(filename, time, open, high, &
                       low, close, adjclose, volume)
  
  character(*), intent(in) :: filename
  character(:), allocatable, intent(inout) :: time(:)
  real, allocatable, intent(inout):: open(:), &
  high(:), low(:), close(:), adjclose(:), volume(:)
  integer :: fileunit
  integer :: n, nm
  
  !! Counts how many lines has the file. doesn't count the header. 
  nm = num_records(filename)-1
  
  !! Allocates the arrays to the found number of records. 
  if (allocated(time)) deallocate(time)
  allocate(character(10):: time(nm))
  call alloc(open, nm)
  call alloc(high, nm)
  call alloc(low, nm)
  call alloc(close, nm)
  call alloc(adjclose, nm)
  call alloc(volume, nm)
  
  !! Opens the file again and reads each line storing each element into the nth position
  open(newunit=fileunit, file=filename)
  read(fileunit, fmt=*, end=1) !! When EOF is reached, it jumps to close the file
  do n=1, nm
    read(fileunit, fmt=*, end=1) time(n), open(n), &
    high(n), low(n), close(n), adjclose(n), volume(n)
  end do
  
  1 close(fileunit)

  end subroutine readStock

  !! function to allocate the array a to have n elements
  subroutine alloc(a, n)
    real, allocatable, intent(inout) :: a(:)
    integer, intent(in):: n
    integer :: stat
    character(len=100) :: errmsg

    if (allocated(a)) call myfree(a) !! if allocated, free memory
    allocate(a(n), stat= stat, errmsg=errmsg) !! allocate storing status in stat and erromsg in errmsg
    if (stat>0) error stop errmsg !! if status is not success, stop execution.

  end subroutine alloc

  !! free memory of an allocated array
  subroutine myfree(a)
    real, allocatable, intent(inout) :: a(:)
    integer :: stat
    character(len=100):: errmsg

    if (.not. allocated(a)) return !! if array is not allocated nothing happens
    deallocate(a, stat=stat, errmsg = errmsg) !! else, deallocates storing stat an errmsg
    if (stat>0) error stop errmsg

  end subroutine myfree

  function reverse(arr)
    real, allocatable, intent(in):: arr(:)
    real, allocatable :: reverse(:)
    integer i, ln

    ln = size(arr)
    allocate(reverse(ln))
    
    do i = 1, ln
      reverse(i) = arr(ln+1-i)
    end do

  end function reverse

end module ChapterFive
