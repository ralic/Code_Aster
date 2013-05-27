        interface
          subroutine lc0006(fami,kpg,ksp,ndim,imate,compor,crit,instam&
     &,instap,neps,epsm,deps,nsig,sigm,vim,option,angmas,sigp,vip,nwkin,&
     &wkin,typmod,icomp,nvi,ndsde,dsidep,nwkout,wkout,codret)
            integer :: nwkout
            integer :: nwkin
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            integer :: neps
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            integer :: nsig
            real(kind=8) :: sigm(*)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: angmas(*)
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: wkin(nwkin)
            character(len=8) :: typmod(2)
            integer :: icomp
            integer :: nvi
            integer :: ndsde
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: wkout(nwkout)
            integer :: codret
          end subroutine lc0006
        end interface
