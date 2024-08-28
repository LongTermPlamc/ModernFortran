program main
  use iso_fortran_env,only: int32, real32
  use ChapterFive,only: readStock, alloc, free, reverse
  implicit none

  integer :: n
  character(len=:),allocatable :: fileDir, fileName
  character(len=4), allocatable :: tags(:)
  character(len=:), allocatable :: time(:)
  real, allocatable :: open(:), high(:), low(:), close(:), adjclose(:), volume(:)
  real :: gain
  fileDir ="C:/Users/Angel/Documents/Trabajo de Investigación/ModernFortran/ChapterFive/stock-prices/data"
  tags =["AAPL","AMZN","CRAY","CSCO","HPQ ",&
         "IBM ","INTC","MSFT","NVDA","ORCL"]
  
  do n = 1, size(tags)
    fileName = fileDir//"/" // trim(tags(n))//".csv"
    call readStock(fileName, time, open, high, low, close,adjclose, volume)
    adjclose = reverse(adjclose)
    gain = adjclose(size(adjclose))- adjclose(1)

    if (n == 1) then
      print *, time(size(time)) // ' through ' // time(1)
      print *, 'Symbol, Gain (USD), Relative gain (%)'
      print *, '-------------------------------------'
    end if
    
    print *, tags(n), gain, nint(gain / adjclose(1) * 100)
  end do

end program main
