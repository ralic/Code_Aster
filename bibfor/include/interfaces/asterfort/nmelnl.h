        interface
          subroutine nmelnl(fami,kpg,ksp,poum,ndim,typmod,imate,compor&
     &,crit,option,eps,sig,vi,dsidep,energi)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(3)
            character(len=16) :: option
            real(kind=8) :: eps(6)
            real(kind=8) :: sig(6)
            real(kind=8) :: vi
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: energi(2)
          end subroutine nmelnl
        end interface
