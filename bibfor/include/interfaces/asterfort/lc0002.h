        interface
          subroutine lc0002(fami,kpg,ksp,ndim,imate,compor,crit,instam&
     &,instap,neps,epsm,deps,nsig,sigm,vim,option,sigp,vip,typmod,ndsde,&
     &dsidep,codret)
            integer :: ndsde
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
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: sigp(nsig)
            real(kind=8) :: vip(*)
            character(len=8) :: typmod(*)
            real(kind=8) :: dsidep(ndsde)
            integer :: codret
          end subroutine lc0002
        end interface
