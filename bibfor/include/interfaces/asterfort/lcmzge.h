        interface
          subroutine lcmzge(fami,kpg,ksp,ndim,typmod,imate,epstm,depst&
     &,vim,option,sig,vip,dsidpt,proj)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            real(kind=8) :: epstm(12)
            real(kind=8) :: depst(12)
            real(kind=8) :: vim(4)
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidpt(6,6,2)
            real(kind=8) :: proj(6,6)
          end subroutine lcmzge
        end interface
