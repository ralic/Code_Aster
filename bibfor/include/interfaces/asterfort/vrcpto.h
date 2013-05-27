        interface
          subroutine vrcpto(compor,deps,neps,fami,kpg,ksp,imate)
            integer :: neps
            character(len=16) :: compor(*)
            real(kind=8) :: deps(neps)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
          end subroutine vrcpto
        end interface
