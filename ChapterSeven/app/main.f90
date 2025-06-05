program main
  use ChapterSeven, only: readData
  use ChapterFive, only: alloc, myfree
  use pyplot_module
  
  implicit none
  type(pyplot) :: plt
  character(40) :: filename
  real, allocatable :: x(:) 
  character(len=20), allocatable :: time(:)
  real, allocatable :: wind_speed(:), air_pressure(:), air_temp(:), &
                       dew(:), water_temp(:), wave_height(:), wave_period(:)
  integer :: len, i
  filename = ".\\data\\buoy_42001.csv"

  write(*,*) filename
  call readData(trim(filename), len, time, wind_speed, air_pressure, air_temp, dew, water_temp, wave_height, wave_period)

  write(*,*) len
  allocate(x(len))
  x=[(real(i), i=1, len)]

  !! big limitant, As this is writing a python script, one cannot plot all points.
  call plt%initialize(grid=.true., xlabel="index", title="TestPlot", legend=.true.)
  call plt%add_plot(x(96694:97436), wind_speed(96694:97436), label="windSpeed", linestyle="b-o", markersize=5,linewidth=2)
  call plt%savefig("windspeed.png", pyfile="windSpeed.py")

  call myfree(wind_speed)
  call myfree(air_pressure)
  call myfree(air_temp)
  call myfree(dew)
  call myfree(water_temp)
  call myfree(wave_height)
  call myfree(wave_period)
  
end program main
