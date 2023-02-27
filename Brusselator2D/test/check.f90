program check
use Brusselator2D

implicit none
real(8), dimension(25) :: numbers
real(8),dimension(5,5) :: diffusion
real(8):: a
integer :: i


print *, "Put some tests in here!"
numbers = [(i,i=1,25)]
a = 10.0d0

numbers(13) = 100

write(*,"(5f10.3)") numbers+a
diffusion = reshape(numbers,(/5,5/))
diffusion = diffuser_twod(diffusion)

write(*,"(5f10.3)") diffusion



end program check
