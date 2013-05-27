        interface
          subroutine lkilnf(nvi,vind,nmat,materf,dt,sigd,nr,yd,yf,deps&
     &,vinf)
            integer :: nmat
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: dt
            real(kind=8) :: sigd(6)
            integer :: nr
            real(kind=8) :: yd(*)
            real(kind=8) :: yf(*)
            real(kind=8) :: deps(6)
            real(kind=8) :: vinf(*)
          end subroutine lkilnf
        end interface
