        interface
          subroutine nmtama(fami,kpg,ksp,imate,instam,instap,matm,mat)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: matm(3)
            real(kind=8) :: mat(14)
          end subroutine nmtama
        end interface
