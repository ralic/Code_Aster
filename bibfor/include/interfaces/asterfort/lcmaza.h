        interface
          subroutine lcmaza(fami,kpg,ksp,ndim,typmod,imate,compor,epsm&
     &,deps,vim,tm,tp,tref,option,sig,vip,dsidep)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(2)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(4)
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: tref
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,6)
          end subroutine lcmaza
        end interface
