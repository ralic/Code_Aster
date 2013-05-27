        interface
          subroutine lcldsb(fami,kpg,ksp,ndim,typmod,imate,compor,epsm&
     &,deps,vim,tm,tp,tref,option,sig,vip,dsidep,crit)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(*)
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: tref
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,12)
            real(kind=8) :: crit(*)
          end subroutine lcldsb
        end interface
