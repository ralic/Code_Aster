        interface
          subroutine d1madp(fami,mater,instan,poum,kpg,ksp,repere,d1)
            character(*) :: fami
            integer :: mater
            real(kind=8) :: instan
            character(*) :: poum
            integer :: kpg
            integer :: ksp
            real(kind=8) :: repere(7)
            real(kind=8) :: d1(4,*)
          end subroutine d1madp
        end interface
