        interface
          subroutine burjac(mod,nmat,materd,materf,nvi,vind,timed,&
     &timef,yd,yf,dy,nr,drdy)
            integer :: nr
            integer :: nvi
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: dy(nr)
            real(kind=8) :: drdy(nr,nr)
          end subroutine burjac
        end interface
