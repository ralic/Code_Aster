        interface
          subroutine nmvprk(fami,kpg,ksp,ndim,typmod,imat,comp,crit,&
     &timed,timef,neps,epsdt,depst,sigd,vind,opt,angmas,sigf,vinf,dsde,&
     &iret)
            integer :: neps
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imat
            character(len=16) :: comp(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: epsdt(neps)
            real(kind=8) :: depst(neps)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            character(len=16) :: opt
            real(kind=8) :: angmas(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            real(kind=8) :: dsde(6,6)
            integer :: iret
          end subroutine nmvprk
        end interface
