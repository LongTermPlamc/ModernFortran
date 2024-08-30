program main
  use iso_fortran_env,only: int32, real32
  use ChapterFive,only: readStock, alloc, free, reverse, mean, std, movingAverage, movingStd, writeStock
  implicit none

  integer :: n, len
  character(len=:),allocatable :: fileDir, writeDir, fileName, newFileName
  character(len=4), allocatable :: tags(:)
  character(len=:), allocatable :: time(:)
  real, allocatable :: open(:), high(:), low(:), close(:), adjclose(:), volume(:)
  real, allocatable ::  price(:), movAvg(:), movStd(:)
  real :: gain, meanOfArray, stdOfArray
  fileDir ="C:/Users/Angel/Documents/Trabajo de Investigación/ModernFortran/ChapterFive/stock-prices/data"
  writeDir ="C:/Users/Angel/Documents/Trabajo de Investigación/ModernFortran/ChapterFive/myData"
  tags =["AAPL","AMZN","CRAY","CSCO","HPQ ",&
         "IBM ","INTC","MSFT","NVDA","ORCL"]
  !! Relative gain example
  ! do n = 1, size(tags)
  !   fileName = fileDir//"/" // trim(tags(n))//".csv"
  !   call readStock(fileName, time, open, high, low, close,adjclose, volume)
  !   adjclose = reverse(adjclose)
  !   gain = adjclose(size(adjclose))- adjclose(1)

  !   if (n == 1) then
  !     print *, time(size(time)) // ' through ' // time(1)
  !     print *, 'Symbol, Gain (USD), Relative gain (%)'
  !     print *, '-------------------------------------'
  !   end if
    
  !   print *, tags(n), gain, nint(gain / adjclose(1) * 100)
  ! end do

  !! Volatility analisys
  do n=1, size(tags)
    fileName = fileDir//"/" // trim(tags(n))//".csv"
    newFileName = writeDir//"/" // trim(tags(n))//"Volatility.csv"
    call readStock(filename, time, open,high, low, close, adjclose, volume)

    len = size(time)

    call alloc(price, len)
    price = reverse(adjclose)
    call alloc(movAvg, len)
    movAvg = movingAverage(adjclose, 30)
    call alloc(movStd, len)
    movStd = movingStd(adjclose, 30)
    
    time = time(len:1:-1)
    call writeStock(newFileName, time , price, movAvg, movStd)

    call free(price)
    call free(movAvg)
    call free(movStd)
  end do

end program main
