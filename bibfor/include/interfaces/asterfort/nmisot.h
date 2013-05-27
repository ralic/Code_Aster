        interface
          subroutine nmisot(fami,kpg,ksp,ndim,typmod,imate,compor,crit&
     &,deps,sigm,vim,option,sigp,vip,dsidep,demu,cinco,iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor
            real(kind=8) :: crit(11)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: demu
            real(kind=8) :: cinco
            integer :: iret
          end subroutine nmisot
        end interface
