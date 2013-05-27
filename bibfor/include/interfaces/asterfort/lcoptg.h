        interface
          subroutine lcoptg(nmat,mater,nr,nvi,drdy,sigeps,dsde,iret)
            common/tdim/ ndt,ndi
              integer :: ndt
              integer :: ndi
            integer :: nvi
            integer :: nr
            integer :: nmat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: drdy(nr,nr)
            integer :: sigeps
            real(kind=8) :: dsde(6,6)
            integer :: iret
          end subroutine lcoptg
        end interface
