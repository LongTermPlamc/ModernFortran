module ChapterSeven
  use iso_fortran_env
  use ChapterFive, only : num_records, alloc, myfree
  implicit none
  private

  public :: readData, denan
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
    write(*,*) "reading finished"

  endsubroutine readData

  pure function denan(array)
  use ieee_arithmetic, only: ieee_is_nan
  real, allocatable, intent(in):: array(:)
  real, allocatable :: denan(:)
  !! second argument of pack is an array
  !! ieee_is_nan return True is Nan
  denan = pack(array, .not. ieee_is_nan(array))

  end function denan

end module ChapterSeven
