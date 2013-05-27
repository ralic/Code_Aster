        interface
          subroutine lc0050(fami,kpg,ksp,ndim,typmod,imate,compor,crit&
     &,instam,instap,neps,epsm,deps,nsig,sigm,nvi,vim,option,angmas,&
     &nwkin,wkin,icomp,stress,statev,ndsde,dsidep,nwkout,wkout,codret)
            integer :: nwkout
            integer :: nwkin
            integer :: nvi
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
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            integer :: nsig
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: angmas(*)
            real(kind=8) :: wkin(nwkin)
            integer :: icomp
            real(kind=8) :: stress(6)
            real(kind=8) :: statev(nvi)
            integer :: ndsde
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: wkout(nwkout)
            integer :: codret
          end subroutine lc0050
        end interface
