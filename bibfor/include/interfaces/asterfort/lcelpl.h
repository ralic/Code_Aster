        interface
          subroutine lcelpl(mod,loi,nmat,materd,materf,timed,timef,&
     &deps,nvi,vind,vinf,nr,yd,yf,sigd,sigf,drdy)
            integer :: nr
            integer :: nvi
            integer :: nmat
            character(len=8) :: mod
            character(len=16) :: loi
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: deps(6)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: drdy(nr,nr)
          end subroutine lcelpl
        end interface
