        interface
          subroutine lclbr1(fami,kpg,ksp,ndim,typmod,imate,compor,epsm&
     &,deps,vim,option,sig,vip,dsidep)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(2)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(2)
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(2)
            real(kind=8) :: dsidep(6,12)
          end subroutine lclbr1
        end interface
