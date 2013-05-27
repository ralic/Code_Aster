        interface
          subroutine nmchab(fami,kpg,ksp,ndim,typmod,imate,compor,crit&
     &,instam,instap,deps,sigm,vim,option,sigp,vip,dsidep,iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(3)
            real(kind=8) :: crit(10)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine nmchab
        end interface
