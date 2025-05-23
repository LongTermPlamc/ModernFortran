program volatility
  use ChapterFive

  implicit none
  character(len=4), allocatable :: symbols(:)
  character(len=100) :: filename
  character(len=:), allocatable :: time(:)
  real            , allocatable :: open(:), high(:), low(:), &
                                   close(:), adjclose(:), volume(:)
  integer :: m, n
  real :: gain

  symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
             'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

  do n = 1, size(symbols)

    filename = "myData\" // trim(symbols(n)) //".csv"

    call readStock(filename, time, open, high, low, close, adjclose, volume)

    gain = average([1.,2.,3.,4.,5.])
    print *, gain

    gain = std([1.,2.,3.,4.,5.])
    print*, gain

    print*, movingAverage([1.,2.,3.,4.,5.,6.], 3)
    print*, movingStd([1.,2.,3.,4.,5.,6.], 3)

    !! So far I have the records stored.

    ! adjclose = reverse(adjclose)
    ! gain = adjclose(size(adjclose)) - adjclose(1)

    ! if (n == 1) then
    !   print *, time(size(time)) // ' through ' // time(1)
    !   print *, ' Symbol | Gain (USD) | Relative gain (%)'
    !   print *, '----------------------------------------'
    ! end if
    
    ! write(*,'(A4,A5,F13.5,I18)') symbols(n),'     ', gain, nint(gain/adjclose(1) * 100)


  end do
end program volatility