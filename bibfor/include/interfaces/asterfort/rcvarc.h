        interface
          subroutine rcvarc(arret,novrc,poum,fami,kpg,ksp,valvrc,iret)
            character(len=1) :: arret
            character(*) :: novrc
            character(*) :: poum
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            real(kind=8) :: valvrc
            integer :: iret
          end subroutine rcvarc
        end interface
