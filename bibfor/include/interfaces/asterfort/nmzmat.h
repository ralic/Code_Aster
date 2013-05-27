        interface
          subroutine nmzmat(fami,kpg,ksp,ndim,typmod,compor,instam,&
     &instap,epsm,deps,sigm,vim,option,angmas,sigp,vip,dsidep,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            character(len=16) :: compor(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: epsm(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: sigm(*)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: angmas(7)
            real(kind=8) :: sigp(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(*)
            integer :: codret
          end subroutine nmzmat
        end interface
