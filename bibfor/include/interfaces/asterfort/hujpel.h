        interface
          subroutine hujpel(etatd,mod,crit,imat,nmat,materf,angmas,&
     &deps,sigd,nvi,vind,sigf,vinf,iret)
            integer :: nmat
            character(len=7) :: etatd
            character(len=8) :: mod
            real(kind=8) :: crit(*)
            integer :: imat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: angmas(3)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            integer :: iret
          end subroutine hujpel
        end interface
