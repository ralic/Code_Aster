        interface
          subroutine hujres(mod,crit,mater,imat,nvi,deps,sigd,vind,&
     &sigf,vinf,iret,etatf)
            character(len=8) :: mod
            real(kind=8) :: crit(*)
            real(kind=8) :: mater(22,2)
            integer :: imat
            integer :: nvi
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            integer :: iret
            character(len=7) :: etatf
          end subroutine hujres
        end interface
