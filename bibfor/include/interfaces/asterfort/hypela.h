        interface
          subroutine hypela(fami,kpg,ksp,poum,ndim,typmod,imate,compor&
     &,crit,eps,sig,dsidep,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: eps(6)
            real(kind=8) :: sig(6)
            real(kind=8) :: dsidep(6,6)
            integer :: codret
          end subroutine hypela
        end interface
