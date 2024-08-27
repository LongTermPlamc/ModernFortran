module modDiff
  use iso_fortran_env, only: int32, real32
  implicit none
  private
  public :: diffCentered

contains

pure function diff(x) result (dx)
  real(real32), intent(in) :: x(:)
  real(real32) :: dx(size(x))
  integer(int32) :: im
  im = size(x)
  dx(1) = x(1) - x(im)
  dx(2:im) = x(2:im) - x(1:im-1)
end function diff

pure function diffCentered(x) result (dx)
  real(real32), intent(in):: x(:)
  real(real32) :: dx(size(x))
  integer(int32) :: im

  im = size(x)
  dx(1) = x(2) - x(im)
  dx(im) = x(1) - x(im-1)
  dx(2:im-1) = x(3:im) - x(1:im-2)
  dx = 0.5 * dx
end function diffCentered

end module modDiff
