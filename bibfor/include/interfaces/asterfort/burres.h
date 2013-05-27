        interface
          subroutine burres(typmod,nmat,materd,materf,timed,timef,nvi,&
     &vin,yd,yf,deps,dy,nr,r)
            integer :: nr
            integer :: nvi
            integer :: nmat
            character(len=8) :: typmod
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: vin(nvi)
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: deps(6)
            real(kind=8) :: dy(nr)
            real(kind=8) :: r(nr)
          end subroutine burres
        end interface
