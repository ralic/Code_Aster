        interface
          subroutine hypmat(fami,kpg,ksp,poum,imate,c10,c01,c20,k)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: imate
            real(kind=8) :: c10
            real(kind=8) :: c01
            real(kind=8) :: c20
            real(kind=8) :: k
          end subroutine hypmat
        end interface
