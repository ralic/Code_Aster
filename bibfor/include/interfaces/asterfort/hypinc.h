        interface
          subroutine hypinc(fami,kpg,ksp,poum,ndim,typmod,imate,compor&
     &,crit,epsm,deps,sigm,sigp,dsidep,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: sigp(6)
            real(kind=8) :: dsidep(6,6)
            integer :: codret
          end subroutine hypinc
        end interface
