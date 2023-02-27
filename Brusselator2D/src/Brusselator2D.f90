module Brusselator2D
  implicit none
  
contains
  subroutine say_hello
    print *, "Hello, Brusselator2D!"
  end subroutine say_hello

  elemental function activator(A,B,m_array,n_array)
    implicit none
    real(8) :: activator
    real(8), intent(in) :: A,B
    real(8), intent(in):: m_array, n_array

    !allocate(activator(size(m_array)))

    activator = A - (B + 1)*m_array + n_array*m_array**2

  end function activator

  elemental function inhibitor(B,m_array,n_array)
    implicit none
    real(8) :: inhibitor
    real(8), intent(in) :: B
    real(8), intent(in):: m_array, n_array

    !allocate(activator(size(m_array)))
    inhibitor = B*m_array - n_array*m_array**2

  end function inhibitor

  function diffuser_oned(grid)
    implicit none
    real(8), dimension(:), allocatable:: diffuser_oned
    real(8), dimension(:):: grid
    integer :: xdim

    xdim = size(grid)
    allocate(diffuser_oned(xdim))
    
    diffuser_oned(2:xdim-1) = grid(1:xdim-2) + grid(3:xdim)- 2* grid(2:xdim-1)
    diffuser_oned(1) = grid(1)
    diffuser_oned(xdim) = grid(xdim)

  end function diffuser_oned

  function diffuser_twod(grid)
    implicit none
    real(8), dimension(:,:), allocatable:: diffuser_twod
    real(8), dimension(:,:):: grid
    integer :: xdim, ydim

    xdim = size(grid,1)
    ydim = size(grid,2)
    allocate(diffuser_twod(xdim,ydim))
    
    diffuser_twod(2:xdim-1,2:ydim-1) = (grid(1:xdim-2,2:ydim-1) + grid(3:xdim,2:ydim-1) + &
                              grid(2:xdim-1,1:ydim-2) + grid(2:xdim-1,3:ydim) - &
                              4*grid(2:xdim-1,2:ydim-1))

    diffuser_twod(1,:) = grid(1,:)
    diffuser_twod(xdim,:) = grid(xdim,:)
    diffuser_twod(:,1) = grid(:,1)
    diffuser_twod(:,ydim) = grid(:,ydim)

  end function diffuser_twod

end module Brusselator2D
