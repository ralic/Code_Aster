        interface
          subroutine lc0046(fami,kpg,ksp,ndim,imate,compor,crit,instam&
     &,instap,neps,epsm,deps,nsig,sigm,vim,option,angmas,sigp,vip,nwkin,&
     &wkin,typmod,icomp,nvi,ndsde,dsidep,nwkout,wkout,codret)
            integer :: nwkout
            integer :: ndsde
            integer :: nvi
            integer :: nwkin
            integer :: nsig
            integer :: neps
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: epsm(neps)
            real(kind=8) :: deps(neps)
            real(kind=8) :: sigm(nsig)
            real(kind=8) :: vim(nvi)
            character(len=16) :: option
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigp(nsig)
            real(kind=8) :: vip(nvi)
            real(kind=8) :: wkin(nwkin)
            character(len=8) :: typmod(*)
            integer :: icomp
            real(kind=8) :: dsidep(ndsde)
            real(kind=8) :: wkout(nwkout)
            integer :: codret
          end subroutine lc0046
        end interface
