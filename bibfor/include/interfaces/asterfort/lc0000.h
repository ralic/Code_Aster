        interface
          subroutine lc0000(fami,kpg,ksp,ndim,typmod,imate,compor,crit&
     &,instam,instap,neps,epsm,deps,nsig,sigm,vim,option,angmas,nwkin,&
     &wkin,cp,numlc,tempd,tempf,tref,sigp,vip,ndsde,dsidep,icomp,nvi,&
     &nwkout,wkout,codret)
            integer :: nwkout
            integer :: nvi
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
            real(kind=8) :: epsm(neps)
            real(kind=8) :: deps(neps)
            real(kind=8) :: sigm(nsig)
            real(kind=8) :: vim(nvi)
            character(len=16) :: option
            real(kind=8) :: angmas(3)
            real(kind=8) :: wkin(nwkin)
            logical :: cp
            integer :: numlc
            real(kind=8) :: tempd
            real(kind=8) :: tempf
            real(kind=8) :: tref
            real(kind=8) :: sigp(nsig)
            real(kind=8) :: vip(nvi)
            real(kind=8) :: dsidep(ndsde)
            integer :: icomp
            real(kind=8) :: wkout(nwkout)
            integer :: codret
          end subroutine lc0000
        end interface
