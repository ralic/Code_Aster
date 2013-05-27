        interface
          subroutine gdclel(fami,kpg,ksp,poum,imate,young,nu)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=1) :: poum
            integer :: imate
            real(kind=8) :: young
            real(kind=8) :: nu
          end subroutine gdclel
        end interface
