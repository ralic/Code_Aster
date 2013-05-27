        interface
          subroutine lcmzcp(fami,kpg,ksp,ndim,imate,epsm,deps,vim,tm,&
     &tp,tref,option,sig,vip,dsidep)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            integer :: imate
            real(kind=8) :: epsm(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: vim(*)
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: tref
            character(len=16) :: option
            real(kind=8) :: sig(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,6)
          end subroutine lcmzcp
        end interface
