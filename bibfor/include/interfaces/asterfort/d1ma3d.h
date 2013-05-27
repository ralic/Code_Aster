        interface
          subroutine d1ma3d(fami,mater,instan,poum,kpg,ksp,repere,&
     &xyzgau,d1)
            character(*) :: fami
            integer :: mater
            real(kind=8) :: instan
            character(*) :: poum
            integer :: kpg
            integer :: ksp
            real(kind=8) :: repere(7)
            real(kind=8) :: xyzgau(3)
            real(kind=8) :: d1(6,6)
          end subroutine d1ma3d
        end interface
