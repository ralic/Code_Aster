        interface
          subroutine nmcoup(fami,kpg,ksp,ndim,typmod,imat,comp,lcpdb,&
     &crit,timed,timef,neps,epsdt,depst,nsig,sigd,vind,opt,nwkin,wkin,&
     &sigf,vinf,ndsde,dsde,nwkout,wkout,iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imat
            character(len=16) :: comp(*)
            logical :: lcpdb
            real(kind=8) :: crit(*)
            real(kind=8) :: timed
            real(kind=8) :: timef
            integer :: neps
            real(kind=8) :: epsdt(*)
            real(kind=8) :: depst(*)
            integer :: nsig
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            character(len=16) :: opt
            integer :: nwkin
            real(kind=8) :: wkin(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            integer :: ndsde
            real(kind=8) :: dsde(*)
            integer :: nwkout
            real(kind=8) :: wkout(*)
            integer :: iret
          end subroutine nmcoup
        end interface
