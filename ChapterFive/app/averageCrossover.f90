program averageCrossover
    !use ChapterFive
    use mod_io
    use mod_arrays
    use mod_io
    use mod_statistics
    implicit none
    character(len=4), allocatable :: symbols(:)
    character(len=100) :: filename, newFileName
    character(len=:), allocatable :: time(:)
    real            , allocatable :: open(:), high(:), low(:), &
                                    close(:), adjclose(:), volume(:)
    real            , allocatable :: movAverage(:), movStd(:)
    integer, allocatable::buy(:), sell(:)
    integer :: m, n, i, newUnit

    symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
             'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

    do i = 1, size(symbols)

        filename = ".\myData\"//trim(symbols(i))//".csv"
        call readStock(filename,time, open, high, low, close, adjclose, volume)

        allocate(buy(size(time)), sell(size(time)))

        time = time(size(time):1:-1)
        adjclose = reverse(adjclose)

        buy = crossPoss(adjClose, 30)
        sell = crossNeg(adjClose, 30)

        newFileName = ".\myData\"//trim(symbols(i))//"_buyAndSell.txt"
        open(newunit = newUnit, file = newFileName)
        do m = 1, size(buy)
            write(newUnit,*) "Buy", "     ", time(buy(m))
        end do
        do m = 1, size(sell)
            write(newUnit,*) "Sell", "     ", time(sell(m))
        end do
        close(newUnit)

        deallocate(buy, sell)

    
    end do


end program