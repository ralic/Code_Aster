        interface
          subroutine nmisex(fami,kpg,ksp,ndim,imate,compor,crit,instam&
     &,instap,deps,sigm,vim,option,sigp,vip,typmod,dsidep,iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(*)
            character(len=8) :: typmod(*)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine nmisex
        end interface
