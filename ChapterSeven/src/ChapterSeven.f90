module ChapterSeven
  use iso_fortran_env
  use ChapterFive, only : num_records, alloc, myfree
  implicit none
  private

  public :: readData, denan, tile_indices, tile_neighbours
contains
  subroutine readData(filename, time, wind_speed)
    character(*), intent(in) :: filename
    character(*), intent(inout),allocatable :: time(:)
    real, intent(inout), allocatable :: wind_speed(:)
    integer :: fileunit
    integer :: n, nm
    
    !! get number of records
    nm = num_records(filename)
    if (allocated(time)) deallocate(time)
    
    !! Only time and wind_speed for now.
    allocate(character(20)::time(nm))
    call alloc(wind_speed, nm)
    
    !! Open file and read line by line. Only taking the first two elements.
    !! By doing this, fortran discards any other element beyond the second one
    open(newunit=fileunit, file=filename)
    do n = 1, nm
      read(fileunit, fmt=*, end=100) time(n), wind_speed(n)
    end do
    100 close(fileunit)
    !write(*,*) "reading finished from image: ", this_image()

  endsubroutine readData

  pure function denan(array)
    use ieee_arithmetic, only: ieee_is_nan
    real, allocatable, intent(in):: array(:)
    real, allocatable :: denan(:)
    !! second argument of pack is an array
    !! ieee_is_nan return True is Nan
    denan = pack(array, .not. ieee_is_nan(array))

  end function denan

  pure function tile_indices(dims)
    ! Given input global array size, return start and end index
    ! of a parallel 1-d tile that correspond to this image.
    integer, intent(in) :: dims
    integer :: tile_indices(2)
    integer :: offset, tile_size

    tile_size = dims / num_images()

    ! start and end indices assuming equal tile sizes
    tile_indices(1) = (this_image() - 1) * tile_size + 1 !! 3 -> 5, 6
    tile_indices(2) = tile_indices(1) + tile_size - 1 !!  4 -> 7, 8

    ! if we have any remainder, distribute it to the tiles at the end 
    offset = num_images() - mod(dims, num_images()) ! 2
    if (this_image() > offset) then
      tile_indices(1) = tile_indices(1) + this_image() - offset - 1 !5  !8
      tile_indices(2) = tile_indices(2) + this_image() - offset ! 7  !10
    end if

  end function tile_indices

  function tile_neighbours()
    integer :: leftN, rightN
    integer:: tile_neighbours(2)

    if (num_images() == 1) then
      leftN = 1
      rightN = 1
    else if (num_images()>1) then
      if (this_image() == 1) then
        leftN = num_images()
      else
        leftN = this_image() -1
      endif
      if (this_image()==num_images())then
        rightN = 1
      else
        rightN = this_image() +1
      endif
    end if

    tile_neighbours = [leftN, rightN]

  end function tile_neighbours

end module ChapterSeven
