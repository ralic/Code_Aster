        interface
          subroutine lcelas(loi,mod,imat,nmat,materd,materf,matcst,nvi&
     &,angmas,deps,sigd,vind,sigf,vinf,theta,etatd,crit,iret)
            integer :: nmat
            character(len=16) :: loi
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            integer :: nvi
            real(kind=8) :: angmas(3)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            real(kind=8) :: theta
            character(len=7) :: etatd
            real(kind=8) :: crit(*)
            integer :: iret
          end subroutine lcelas
        end interface
