        interface
          subroutine hujjac(mod,nmat,mater,indi,deps,nr,yd,yf,ye,nvi,&
     &vind,vins,vinf,drdy,bnews,mtrac,iret)
            integer :: nvi
            integer :: nr
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: mater(nmat,2)
            integer :: indi(7)
            real(kind=8) :: deps(6)
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: ye(nr)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vins(nr)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: drdy(nr,nr)
            logical :: bnews(3)
            logical :: mtrac
            integer :: iret
          end subroutine hujjac
        end interface
