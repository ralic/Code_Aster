        interface
          subroutine nmgran(fami,kpg,ksp,typmod,imate,compor,instam,&
     &instap,tpmxm,tpmxp,depst,sigm,vim,option,sigp,vip,dsidep)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(3)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: tpmxm
            real(kind=8) :: tpmxp
            real(kind=8) :: depst(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(55)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(55)
            real(kind=8) :: dsidep(6,6)
          end subroutine nmgran
        end interface
