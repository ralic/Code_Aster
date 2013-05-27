        interface
          subroutine huresi(mod,nmat,mater,indi,deps,nr,yd,yf,nvi,vind&
     &,r,iret)
            integer :: nvi
            integer :: nr
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: mater(nmat,2)
            integer :: indi(7)
            real(kind=8) :: deps(6)
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: r(*)
            integer :: iret
          end subroutine huresi
        end interface
