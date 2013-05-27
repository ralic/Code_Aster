        interface
          subroutine redece(fami,kpg,ksp,ndim,typmod,imate,compor,crit&
     &,instam,instap,neps,epsdt,depst,nsig,sigd,vind,option,angmas,nwkin&
     &,wkin,cp,numlc,tempd,tempf,tref,sigf,vinf,ndsde,dsde,nwkout,wkout,&
     &codret)
            integer :: nwkout
            integer :: ndsde
            integer :: nwkin
            integer :: nsig
            integer :: neps
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: epsdt(neps)
            real(kind=8) :: depst(neps)
            real(kind=8) :: sigd(nsig)
            real(kind=8) :: vind(*)
            character(len=16) :: option
            real(kind=8) :: angmas(*)
            real(kind=8) :: wkin(nwkin)
            logical :: cp
            integer :: numlc
            real(kind=8) :: tempd
            real(kind=8) :: tempf
            real(kind=8) :: tref
            real(kind=8) :: sigf(nsig)
            real(kind=8) :: vinf(*)
            real(kind=8) :: dsde(ndsde)
            real(kind=8) :: wkout(nwkout)
            integer :: codret
          end subroutine redece
        end interface
