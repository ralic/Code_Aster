        interface
          subroutine mmfonf(ndim,nno,alias,ksi1,ksi2,ff,dff,ddff)
            integer :: ndim
            integer :: nno
            character(len=8) :: alias
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: ff(9)
            real(kind=8) :: dff(2,9)
            real(kind=8) :: ddff(3,9)
          end subroutine mmfonf
        end interface
