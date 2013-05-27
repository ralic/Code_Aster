        interface
          subroutine mm2onf(ndim,nno,alias,ksi1,ksi2,ddff)
            integer :: ndim
            integer :: nno
            character(len=8) :: alias
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: ddff(3,9)
          end subroutine mm2onf
        end interface
