program coldFront

    implicit none
    integer :: n
    real :: nhours

    do n = 6, 48, 6
        nhours = real(n)
        print *, 'Temperature after ', nhours, ' hours is ', coldFrontTemperature(12., 24., 20., 960., nhours), ' degrees.'
    end do

    contains

        real function coldFrontTemperature(temp1, temp2, c, dx, dt) result (res)
            real, intent(in) :: temp1, temp2, c, dx, dt
            res = temp2 - c*(temp2 - temp1) / dx * dt
        end function coldFrontTemperature

end program coldFront