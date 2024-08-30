module ChapterFive
  
  implicit none
  private

  public :: say_hello, readStock, alloc, free, reverse, mean, std, movingAverage, movingStd, writeStock
contains
  subroutine say_hello
    print *, "Hello, ChapterFive!"
  end subroutine say_hello

  subroutine readStock(filename, time, open, high, low, close,adjclose, volume)
    character(*), intent(in):: filename
    character(:),allocatable, intent(inout):: time(:)
    real, allocatable, intent(inout) ::open(:),high(:),low(:),close(:),adjclose(:), volume(:)

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

  subroutine writeStock(filename, time, price, movAverage, movStdDev)
    character(*), intent(in) :: filename
    character(len=:), allocatable, intent(in) :: time(:)
    real, intent(in) :: price(:), movAverage(:), movStdDev(:)
    integer ::newUnit, index

    open(newUnit, file=filename, action='write')

    do index =1, size(time)
      write(newUnit, *) time(index), price(index),movAverage(index),movStdDev(index)
    end do
    close(newUnit)

  end subroutine

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

  pure function mean(a)
    real, intent(in) :: a(:)
    real:: mean

    mean = sum(a) / size(a)
  end function mean

  pure real function std(a)
    real, intent(in) :: a(:)
    real :: tempMean

    tempMean = mean(a)

    std = sqrt(mean((a-tempMean) ** 2)) 

  end function std

  function movingAverage(a, win)
    real, intent(in) :: a(:)
    integer, intent(in) :: win
    real :: movingAverage(size(a))
    integer :: i, winSize

    do i = 1, size(a)
      winSize = max(i-win,1)
      movingAverage(i) = mean(a(winSize:i))
    end do
  end function movingAverage

  function movingStd(a, win)
    real, intent(in) :: a(:)
    integer, intent(in) :: win
    real :: movingStd(size(a))
    integer :: i, winSize

    do i = 1, size(a)
      winSize = max(i-win,1)
      movingStd(i) = std(a(winSize:i))
    end do
  end function movingStd


end module ChapterFive
