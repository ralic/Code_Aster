        interface
          subroutine nmcomp(fami,kpg,ksp,ndim,typmod,imate,compor,crit&
     &,instam,instap,neps,epsm,deps,nsig,sigm,vim,option,angmas,nwkin,&
     &wkin,sigp,vip,ndsde,dsidep,nwkout,wkout,codret)
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
            integer :: neps
            real(kind=8) :: epsm(*)
            real(kind=8) :: deps(*)
            integer :: nsig
            real(kind=8) :: sigm(*)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: angmas(*)
            integer :: nwkin
            real(kind=8) :: wkin(*)
            real(kind=8) :: sigp(*)
            real(kind=8) :: vip(*)
            integer :: ndsde
            real(kind=8) :: dsidep(*)
            integer :: nwkout
            real(kind=8) :: wkout(*)
            integer :: codret
          end subroutine nmcomp
        end interface
