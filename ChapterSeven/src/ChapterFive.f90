module ChapterFive
  
  implicit none
  private

  public :: readStock, alloc, myfree, num_records, reverse, average, std, movingAverage, movingStd, writeStock, crossPoss, crossNeg
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
    read(fileunit, fmt=*, end=1) time(n), open(n), high(n), low(n), close(n), adjclose(n), volume(n)
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

  pure real function average(array)
  
  real, intent(in):: array(:)

  average = sum(array)/size(array)

  end function average

  pure real function std(array)
  real, intent(in) :: array(:)

  std = sqrt(average((array-average(array))**2))
  end function std

  function movingAverage(array, window)
  real, intent(in) :: array(:)
  integer, intent(in) :: window
  real, allocatable :: movingAverage(:)
  integer :: length, i, upBound, lowBound

  length = size(array)

  call alloc(movingAverage, length)

  do i=1, length
    upBound = i
    lowBound = max(1,i-window+1)
    movingAverage(i) = average( array(lowBound:upBound) )
  end do

  end function movingAverage

  function movingStd(array, window)
  real, intent(in) :: array(:)
  integer, intent(in) :: window
  real, allocatable :: movingStd(:)
  integer :: length, i, upBound, lowBound

  length = size(array)

  call alloc(movingStd, length)

  do i=1, length
    upBound = i
    lowBound = max(1,i-window+1)
    movingStd(i) = std( array(lowBound:upBound) )
  end do
  end function movingStd

  subroutine writeStock(filename, time, adjClose, movAverage, movStd)
    character(*), intent(in):: filename
    character(:), intent(in), allocatable :: time(:)
    real, intent(in), allocatable:: adjClose(:),movAverage(:), movStd(:)

    integer :: len, i, fileunit
    character(40) :: frmt

    frmt = "(A10,f12.6,f12.6,f12.6)"

    open(newunit=fileunit, file=filename)

    len = size(time)
    do i = 1, len
      write(unit =fileunit, fmt= trim(frmt)) time(i), adjClose(i),movAverage(i), movStd(i)
    end do
    
    close(fileunit)

  end subroutine writeStock

  function crossPoss(x, w) result (res)
    real, intent(in):: x(:)
    integer, intent(in) :: w
    integer, allocatable :: res(:)
    logical, allocatable :: greaterThan(:), lessThan(:)
    real, allocatable :: mvgAvg(:), mvgStd(:)
    integer :: i

    allocate(res(size(x-2)), mvgAvg(size(x)), mvgStd(size(x)))
    allocate(greaterThan(size(x)), lessThan(size(x)))

    res= [(i, i =2, size(x))]
    greaterThan = x > movingAverage(x, w)
    lessThan = x < movingAverage(x, w)

    res = pack(res, greaterThan(2:) .and. lessThan(:size(x)-1))
    deallocate(mvgAvg, mvgStd)

  end function crossPoss

  function crossNeg(x, w) result (res)
    real, intent(in):: x(:)
    integer, intent(in) :: w
    integer, allocatable :: res(:)
    logical, allocatable :: greaterThan(:), lessThan(:)
    real, allocatable :: mvgAvg(:), mvgStd(:)
    integer :: i

    allocate(res(size(x-2)), mvgAvg(size(x)), mvgStd(size(x)))
    allocate(greaterThan(size(x)), lessThan(size(x)))

    res= [(i, i =2, size(x))]
    greaterThan = x > movingAverage(x, w)
    lessThan = x < movingAverage(x, w)

    res = pack(res, lessThan(2:) .and. greaterThan(:size(x)-1))
    deallocate(mvgAvg, mvgStd)

  end function crossNeg

end module ChapterFive
