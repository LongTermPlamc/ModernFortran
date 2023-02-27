program main
  use Brusselator2D
  implicit none

  real(8), dimension(5,5) :: m_array, n_array, result
  real(8), dimension(5,5) :: a, b
  integer :: i
  
  call say_hello()
  m_array = reshape([(i,i=1,25)],(/5,5/))
  n_array = reshape([(i,i=1,25)],(/5,5/))
  a = 1.0d0
  b = 1.0d0

  result = activator(a, b, m_array, n_array)
  write(*,*) result

  result = inhibitor(b, m_array, n_array)
  write(*,*) result

end program main
