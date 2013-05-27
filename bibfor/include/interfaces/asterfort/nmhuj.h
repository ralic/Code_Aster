        interface
          subroutine nmhuj(typmod,imat,comp,crit,instam,instap,tempm,&
     &tempf,tref,angmas,epsd,deps,sigd,vind,opt,sigf,vinf,dsde,iret)
            character(len=8) :: typmod(*)
            integer :: imat
            character(len=16) :: comp(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: tempm
            real(kind=8) :: tempf
            real(kind=8) :: tref
            real(kind=8) :: angmas(3)
            real(kind=8) :: epsd(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(50)
            character(len=16) :: opt
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(50)
            real(kind=8) :: dsde(6,6)
            integer :: iret
          end subroutine nmhuj
        end interface
