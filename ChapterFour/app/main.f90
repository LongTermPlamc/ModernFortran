program coldFront
  use iso_fortran_env, only: int32, real32
  use modDiff, only: diff => diffCentered
  use modInitial, only: setGaussian
  implicit none

  integer(int32) :: n
  integer(int32), parameter ::gridSize = 100, timeSteps = 5000
  real(real32), parameter :: dt = 0.02, dx = 1., g = 9.8
  real(real32), parameter :: hmean = 10

  real(real32), dimension(gridSize) :: h, u

  integer(int32), parameter :: icenter = 25
  real(real32), parameter :: decay = 0.02

  if (gridSize <= 0) stop 'grid_size must be > 0'
  if (dt <= 0) stop 'time step dt must be > 0'
  if (dx <= 0) stop 'grid spacing dx must be > 0'
  !!if (c <= 0) stop 'background flow speed c must be > 0'

  call setGaussian(h, icenter, decay)
  u = 0

  print *, 0, h
  timeloop: do n=1, timeSteps
    u = u - (u * diff(u)+ g * diff(h)) / dx * dt
    h = h - diff(u * (hmean + h)) / dx * dt
    print *, n, h
  end do timeloop

  contains
    
    ! pure function diff(x) result(dx)
    !   real(real32), intent(in), dimension(:) :: x
    !   real(real32), dimension(size(x)) :: dx
    !   integer(int32) :: im

    !   im = size(x)
    !   dx(1) = x(1)-x(im)
    !   dx(2:im) = x(2:im)-x(1:im-1)
    ! end function diff

    ! pure subroutine setGaussian(x, icenter, decay)
    !   real(real32), intent(inout) :: x(:)
    !   integer(int32), intent(in) :: icenter 
    !   real(real32), intent(in) :: decay
    !   integer(int32) :: i
      
    !   do concurrent (i = 1:size(x))
    !     x(i) = exp(-decay * (i - icenter)**2)
    !   end do  
    !  end subroutine setGaussian 

  
end program coldFront
