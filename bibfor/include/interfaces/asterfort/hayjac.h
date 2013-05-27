        interface
          subroutine hayjac(mod,nmat,coefel,coeft,timed,timef,yf,deps,&
     &nr,nvi,vind,vinf,yd,dy,crit,drdy,iret)
            integer :: nvi
            integer :: nr
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: coefel(nmat)
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yf(nr)
            real(kind=8) :: deps(6)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: yd(nr)
            real(kind=8) :: dy(nr)
            real(kind=8) :: crit(*)
            real(kind=8) :: drdy(nr,nr)
            integer :: iret
          end subroutine hayjac
        end interface
