        interface
          subroutine hujpre(etat,mod,crit,imat,mater,deps,sigd,sigf,&
     &vind,iret)
            character(len=7) :: etat
            character(len=8) :: mod
            real(kind=8) :: crit(*)
            integer :: imat
            real(kind=8) :: mater(22,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vind(*)
            integer :: iret
          end subroutine hujpre
        end interface
