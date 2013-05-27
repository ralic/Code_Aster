        interface
          subroutine rupmat(fami,kpg,ksp,imat,vim,lgpg,e,sigd)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imat
            real(kind=8) :: vim(*)
            integer :: lgpg
            real(kind=8) :: e
            real(kind=8) :: sigd(6)
          end subroutine rupmat
        end interface
