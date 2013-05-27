        interface
          subroutine lc0037(fami,kpg,ksp,ndim,imate,compor,crit,instam&
     &,instap,neps,epsm,deps,sigm,vim,option,angmas,sigp,vip,typmod,&
     &icomp,nvi,dsidep,codret)
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
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: angmas(*)
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(*)
            character(len=8) :: typmod(*)
            integer :: icomp
            integer :: nvi
            real(kind=8) :: dsidep(6,6)
            integer :: codret
          end subroutine lc0037
        end interface
