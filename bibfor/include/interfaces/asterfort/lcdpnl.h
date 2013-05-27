        interface
          subroutine lcdpnl(fami,kpg,ksp,typmod,ndim,option,compor,&
     &imate,sigm,deps,vim,vip,sig,dsidep,proj,iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: typmod(*)
            integer :: ndim
            character(len=16) :: option
            character(len=16) :: compor(*)
            integer :: imate
            real(kind=8) :: sigm(6)
            real(kind=8) :: deps(6,2)
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: sig(6)
            real(kind=8) :: dsidep(6,6,2)
            real(kind=8) :: proj(6,6)
            integer :: iret
          end subroutine lcdpnl
        end interface
