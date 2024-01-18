program coldFront
  implicit none

  integer :: n
  integer, parameter ::gridSize = 100, timeSteps = 100
  real, parameter :: dt = 1., dx = 1., c = 1.

  real, dimension(gridSize) :: h

  integer, parameter :: icenter = 25
  real, parameter :: decay = 0.02

  if (gridSize <= 0) stop 'grid_size must be > 0'
  if (dt <= 0) stop 'time step dt must be > 0'
  if (dx <= 0) stop 'grid spacing dx must be > 0'
  if (c <= 0) stop 'background flow speed c must be > 0'

  call setGaussian(h, icenter, decay)
  print *, 0, h

  timeloop: do n=1, timeSteps
    h = h - c * diff(h) / dx * dt
    print *, n, h
  end do timeloop

  contains
    
    pure function diff(x) result(dx)
      real, intent(in), dimension(:) :: x
      real, dimension(size(x)) :: dx
      integer :: im

      im = size(x)
      dx(1) = x(1)-x(im)
      dx(2:im) = x(2:im)-x(1:im-1)
    end function diff

    pure subroutine setGaussian(x, icenter, decay)
      real, intent(inout) :: x(:)
      integer, intent(in) :: icenter 
      real, intent(in) :: decay
      integer :: i
      
      do concurrent (i = 1:size(x))
        x(i) = exp(-decay * (i - icenter)**2)
      end do  
     end subroutine setGaussian 

  
end program coldFront
