        interface
          subroutine lcumfp(fami,kpg,ksp,ndim,typmod,imate,compor,&
     &tinstm,tinstp,epsm,deps,sigm,vim,option,sigp,vip,dsidep,crit)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(3)
            real(kind=8) :: tinstm
            real(kind=8) :: tinstp
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(*)
            character(len=16) :: option(2)
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: crit(*)
          end subroutine lcumfp
        end interface
