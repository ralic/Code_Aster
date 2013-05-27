        interface
          subroutine hujopt(mod,angmas,imat,nmat,mater,nvi,vinf,nr,&
     &drdy,sigf,dsde,iret)
            integer :: nr
            integer :: nvi
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: angmas(3)
            integer :: imat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: drdy(nr,nr)
            real(kind=8) :: sigf(6)
            real(kind=8) :: dsde(6,6)
            integer :: iret
          end subroutine hujopt
        end interface
