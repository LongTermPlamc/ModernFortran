program helloParallel
    use ieee_arithmetic, only: ieee_is_nan
    use ChapterSeven, only: readData, denan, tile_indices
    use ChapterFive, only: alloc, myfree, average
    implicit none
    real, allocatable :: array(:)
    character(40) :: filename
    character(len=20), allocatable :: time(:)
    character(5), allocatable:: ids(:)
    integer:: ints(2)
    integer:: i, len, lowIn,higIn
    real, allocatable :: wind_speed(:)
    real, allocatable :: max_wind(:), mean_wind(:) 
    character(:), allocatable :: msg
    character(100) :: line
    real,allocatable :: gather(:)[:] !! allocatable coarray

    !write(*,*) "Hello from image: ", this_image()
    !ints = tile_indices(size(array))

    ids = ['42001','42002','42003','42020','42035','42036','42039','42040','42055']
    ints = tile_indices(size(ids)) !!Tile index,
    lowIn = ints(1) !! get local boundaries and store them
    higIn = ints(2)

    !write(*,*) "Processing following ids:", lowIn, higIn

    allocate(max_wind(lowIn:higIn), mean_wind(lowIn:higIn)) 

    do i =lowIn, higIn
        filename = "./data/buoy_"//trim(ids(i))//".csv"
        call readData(filename, time, wind_speed)
        wind_speed= denan(wind_speed)
        max_wind(i) = maxval(wind_speed)
        mean_wind(i) = average(wind_speed)
    end do

    allocate(gather(size(ids))[*])
    gather(lowIn:higIn)[1] = max_wind
    sync all
    if(this_image() ==1) then
        write(*,*) "Max speed measured is: ", maxval(gather), "measured at station: ", ids(maxloc(gather))
    end if


end program helloParallel

!! All the following is a terrible reasoning. I'll leave here as proff of my dumbness
!! How does this tiling works??
!! [1,2,3,4,5,6,7,8,9] -> 9 elements
!! first, i'll divide this among the number of procesess, ie 3.
!! 9/3  = 3 elements for each process. How many elements left, 0. So.
!! Process 1: 1-3 -> i1 =(1 + numElem*[num_process-1]) i2=(i1 + numElem -1) -> 1,3
!! Process 2: 4-6 -> i1 =(1 + numElem*[num_process-1]) i2=(i1 + numElem -1) -> 4,6
!! Process 3: 7-9 -> i1 =(1 + numElem*[num_process-1]) i2=(i1 + numElem -1) -> 7,9
!! This works for mod(size,numElem) = 0

!! How does this tiling works??
!! [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] -> 15 elements
!! first, i'll divide this among the number of procesess, ie 4.
!! 15/4  = 3 elements for each process. How many elements left, 3. So.
!! Process 1: 1-3 -> i1 =(1 + numElem*[num_process-1]) i2=(i1 + numElem -1) -> 1,3
!! Process 2: 4-6 -> i1 =(1 + numElem*[num_process-1]) i2=(i1 + numElem -1) -> 4,6
!! Process 3: 7-9 -> i1 =(1 + numElem*[num_process-1]) i2=(i1 + numElem -1) -> 7,9
!! Process 4: 10-12 -> i1 =(1 + numElem*[num_process-1]) i2=(i1 + numElem -1) -> 10,12
!! Now I need to distribute 3 elements among the 3 last processes.
!! So the new indexing should look like this:
!! variable(elements, totalProcess) -> 15, 4 -> totalProcess-Mod(elements,totalProcess) = 1 
!! Variable is how many elements This needs to creat a new group
!! variable(15,4) -> 1
!! 1,3 -> If thisProcess < variable: do nothing  -> 1,3
!! 4,6 -> thisProcess > variable: i1 -1 + variable*(num_process-1), i2+ (num_process-1)*variable -> 4,7
!! 7,9 -> thisProcess > variable: i1 -1 + variable*(num_process-1), i2+ (num_process-1)*variable -> 8,11
!! 10,12 -> thisProcess > variable: i1 -1 + variable*(num_process-1), i2+ (num_process-1)*variable -> 12,15
!!
!!       _____       ________
!![1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
!! ____        _____
!!      