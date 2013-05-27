        interface
          subroutine burlnf(nvi,vind,nmat,materd,materf,dt,nr,yd,yf,&
     &vinf,sigf)
            integer :: nr
            integer :: nmat
            integer :: nvi
            real(kind=8) :: vind(nvi)
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: dt
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: sigf(6)
          end subroutine burlnf
        end interface
