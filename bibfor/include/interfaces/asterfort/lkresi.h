        interface
          subroutine lkresi(typmod,nmat,materf,timed,timef,nvi,vind,&
     &vinf,yd,yf,deps,nr,r)
            integer :: nr
            integer :: nmat
            character(len=8) :: typmod
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: deps(6)
            real(kind=8) :: r(nr)
          end subroutine lkresi
        end interface
