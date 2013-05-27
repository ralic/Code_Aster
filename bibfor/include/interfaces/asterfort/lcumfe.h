        interface
          subroutine lcumfe(fami,kpg,ksp,ndim,typmod,imate,tinstm,&
     &tinstp,epstm,depst,sigm,vim,option,sigp,vip,dsidpt,proj)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            real(kind=8) :: tinstm
            real(kind=8) :: tinstp
            real(kind=8) :: epstm(12)
            real(kind=8) :: depst(12)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(25)
            character(len=16) :: option(2)
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(25)
            real(kind=8) :: dsidpt(6,6,2)
            real(kind=8) :: proj(6,6)
          end subroutine lcumfe
        end interface
