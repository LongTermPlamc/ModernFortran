program main
  use ieee_arithmetic, only: ieee_is_nan
  use ChapterSeven, only: readData, denan
  use ChapterFive, only: alloc, myfree, average
  
  implicit none
  character(40) :: filename
  character(5), allocatable:: ids(:)
  real, allocatable :: x(:) 
  character(len=20), allocatable :: time(:)
  real, allocatable :: wind_speed(:)
  real, allocatable :: max_wind(:), mean_wind(:) 
  integer :: len, i
  
  ids = ['42001','42002','42003','42020','42035','42036','42039','42040','42055']
  allocate(max_wind(size(ids)), mean_wind(size(ids)))


  do i =1, size(ids)
    filename = ".\\data\\buoy_"//trim(ids(i))//".csv"
    call readData(filename, time, wind_speed)
    wind_speed= denan(wind_speed)
    max_wind(i) = maxval(wind_speed)
    mean_wind(i) = average(wind_speed)
  end do

  !! here I use functions as maxval, minval, maxloc, minloc 
  !! to check min and max values in the max and mean_wind arrays

  write(*,*) "Max wind speed ", maxval(max_wind)," found at station "&
             ,ids(maxloc(max_wind)),"."
  write(*,*) "Max mean wind speed ", maxval(mean_wind)," found at station "&
             ,ids(maxloc(mean_wind)),"."
  write(*,*) "Min mean wind speed ", minval(mean_wind)," found at station "&
             ,ids(minloc(mean_wind)),"."
  
end program main
