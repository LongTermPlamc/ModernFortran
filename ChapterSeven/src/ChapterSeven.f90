module ChapterSeven
  use iso_fortran_env
  use ChapterFive, only : num_records, alloc, myfree
  implicit none
  private

  public :: readData
contains
  subroutine readData(filename, len, time, wind_speed, &
                      air_pressure, air_temp, dew, water_temp, &
                      wave_height, wave_period)
    character(*), intent(in) :: filename
    integer, intent(out) :: len
    character(*), intent(inout),allocatable :: time(:)
    real, intent(inout), allocatable :: wind_speed(:), air_pressure(:), air_temp(:), &
                                           dew(:), water_temp(:), wave_height(:), wave_period(:)
    integer :: fileunit
    integer :: n, nm

    nm = num_records(filename)
    if (allocated(time)) deallocate(time)
    allocate(character(20)::time(nm))

    call alloc(wind_speed, nm)
    call alloc(air_pressure, nm)
    call alloc(air_temp, nm)
    call alloc(dew, nm)
    call alloc(water_temp, nm)
    call alloc(wave_height, nm)
    call alloc(wave_period, nm)

    len = nm
    open(newunit=fileunit, file=filename)
    do n = 1, nm
      read(fileunit, fmt=*, end=100) time(n), wind_speed(n), air_pressure(n), &
                                   air_temp(n), dew(n), water_temp(n), wave_height(n), &
                                   wave_period(n)
    end do

    100 close(fileunit)
    write(*,*) "reading finished"

  endsubroutine readData
end module ChapterSeven
