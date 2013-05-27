        interface
          subroutine mmnonf(ndim,nno,alias,ksi1,ksi2,ff)
            integer :: ndim
            integer :: nno
            character(len=8) :: alias
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: ff(9)
          end subroutine mmnonf
        end interface
