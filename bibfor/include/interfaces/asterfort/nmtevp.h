        interface
          subroutine nmtevp(fami,kpg,ksp,ndim,typmod,imate,compor,crit&
     &,instam,instap,deps,sigm,vim,option,sigp,vip,dsidep,demu,cinco,&
     &iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(3)
            real(kind=8) :: crit(6)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(5)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(5)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: demu
            real(kind=8) :: cinco
            integer :: iret
          end subroutine nmtevp
        end interface
