program volatility
  use ChapterFive

  implicit none
  character(len=4), allocatable :: symbols(:)
  character(len=100) :: filename, newFileName
  character(len=:), allocatable :: time(:)
  real            , allocatable :: open(:), high(:), low(:), &
                                   close(:), adjclose(:), volume(:)
  real            , allocatable :: movAverage(:), movStd(:)
  integer :: m, n

  symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
             'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

  do n = 1, size(symbols)

    filename = "data/" // trim(symbols(n)) //".csv"
    newFileName = "data/" // trim(symbols(n)) //"_volatility.csv"

    call readStock(filename, time, open, high, low, close, adjclose, volume)

    time  = time(size(time):1:-1)
    adjclose = reverse(adjclose)

    !! So far I have the records stored.
    allocate(movAverage(size(time)), movStd(size(time)))

    movAverage = movingAverage(adjclose, 30)
    movStd = movingStd(adjclose,30)

    call writeStock(trim(newFileName), time, adjclose, movAverage, movStd)

    deallocate(movAverage, movStd)


  end do
end program volatility