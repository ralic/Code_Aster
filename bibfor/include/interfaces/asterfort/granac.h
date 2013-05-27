        interface
          subroutine granac(fami,kpg,ksp,icdmat,materi,compo,irrap,&
     &irram,tm,tp,depsgr)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: icdmat
            character(len=8) :: materi
            character(len=16) :: compo
            real(kind=8) :: irrap
            real(kind=8) :: irram
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: depsgr
          end subroutine granac
        end interface
