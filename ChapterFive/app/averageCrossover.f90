program averageCrossover
    use ChapterFive

    implicit none
    character(len=4), allocatable :: symbols(:)
    character(len=100) :: filename, newFileName
    character(len=:), allocatable :: time(:)
    real            , allocatable :: open(:), high(:), low(:), &
                                    close(:), adjclose(:), volume(:)
    real            , allocatable :: movAverage(:), movStd(:)
    logical         , allocatable :: greaterThan(:), lessThan(:)
    integer, allocatable::res(:)
    integer :: m, n, i

    symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
             'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

    do i = 1, 1

        filename = "./data/"//trim(symbols(i))//".csv"
        call readStock(filename,time, open, high, low, close, adjclose, volume)

        time = time(size(time):1:-1)
        adjclose = reverse(adjclose)
        movAverage = movingAverage(adjclose, 30)
        allocate(greaterThan(size(time)))
        allocate(lessThan(size(time)))
        allocate(res(size(time)))

        greaterThan = adjclose > movAverage
        lessThan = adjclose < movAverage

        res = pack(adjclose(2:),greaterThan(2:).and. lessThan(:size(time)-1))

        print*, res

       
        deallocate(greaterThan,lessThan,res)
    end do


end program