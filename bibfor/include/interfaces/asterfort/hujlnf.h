        interface
          subroutine hujlnf(toler,nmat,mater,nvi,vind,vinf,vins,nr,yd,&
     &yf,sigd,sigf,indi,iret)
            integer :: nr
            integer :: nvi
            integer :: nmat
            real(kind=8) :: toler
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: vins(nvi)
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            integer :: indi(7)
            integer :: iret
          end subroutine hujlnf
        end interface
