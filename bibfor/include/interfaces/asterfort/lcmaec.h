        interface
          subroutine lcmaec(fami,kpg,ksp,poum,nmater,imat,necoul,nbval&
     &,valres,nmat)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            character(len=16) :: nmater
            integer :: imat
            character(len=16) :: necoul
            integer :: nbval
            real(kind=8) :: valres(nmat)
          end subroutine lcmaec
        end interface
