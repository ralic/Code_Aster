        interface
          subroutine mmdonf(ndim,nno,alias,ksi1,ksi2,dff)
            integer :: ndim
            integer :: nno
            character(len=8) :: alias
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: dff(2,9)
          end subroutine mmdonf
        end interface
