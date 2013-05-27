        interface
          subroutine lkijac(mod,nmat,materf,timed,timef,yf,deps,nr,nvi&
     &,vind,vinf,yd,dy,drdy,iret)
            integer :: nvi
            integer :: nr
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yf(nr)
            real(kind=8) :: deps(6)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: yd(nr)
            real(kind=8) :: dy(nr)
            real(kind=8) :: drdy(nr,nr)
            integer :: iret
          end subroutine lkijac
        end interface
