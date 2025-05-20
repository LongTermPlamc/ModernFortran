program stockGain
  use ChapterFive

  implicit none
  character(len=4), allocatable :: symbols(:)
  character(len=100) :: filename
  character(len=:), allocatable :: time(:)
  real            , allocatable :: open(:), high(:), low(:), &
                                   close(:), adjclose(:), volume(:)
  integer :: m, n, nm_rcrd
  real :: gain

  symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
             'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

  do n = 1, size(symbols)

    filename = "myData\" // trim(symbols(n)) //".csv"

    call readStock(filename, time, open, high, low, close, adjclose, volume)
    adjclose = reverse(adjclose)
    gain = adjclose(size(adjclose)) - adjclose(1)

    if (n == 1) then
      print *, time(size(time)) // ' through ' // time(1)
      print *, ' Symbol | Gain (USD) | Relative gain (%)'
      print *, '----------------------------------------'
    end if
    
    write(*,'(A4,A5,F13.5,I18)') symbols(n),'     ', gain, nint(gain/adjclose(1) * 100)


  end do
end program stockGain