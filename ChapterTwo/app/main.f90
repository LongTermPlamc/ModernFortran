program tsunami

  implicit none
  
  integer :: i,n
  integer, parameter :: gridSize = 100
  integer, parameter :: numTimeSteps = 100
  
  real, parameter :: dt = 1., dx = 1., c = 1.

  real, dimension(gridSize) :: h, dh

  integer, parameter :: icenter = 25
  real, parameter :: decay = 0.02

  ! check input parameter values
  if (gridSize <= 0) stop 'grid_size must be > 0'
  if (dt <= 0) stop 'time step dt must be > 0'
  if (dx <= 0) stop 'grid spacing dx must be > 0'
  if (c <= 0) stop 'background flow speed c must be > 0'

  do concurrent(i = 1:gridSize)
    h(i) = exp(-decay * (i - icenter)**2)
  end do

  print *, 0, h

  time_loop: do n = 1, numTimeSteps
    
    dh(1) = h(1) - h(gridSize)
    
    dhLoop: do concurrent (i = 2: gridSize)
      dh(i) = h(i) - h(i-1)
    end do dhLoop

    hLoop: do concurrent (i = 1: gridSize)
      h(i) = h(i) - c * dh(i) / dx * dt
    end do hLoop

    print *, n, h

  end do time_loop

end program tsunami