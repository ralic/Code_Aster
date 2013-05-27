        interface
          subroutine rsliso(fami,kpg,ksp,poum,imat,p,rp,drdp)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=1) :: poum
            integer :: imat
            real(kind=8) :: p
            real(kind=8) :: rp
            real(kind=8) :: drdp
          end subroutine rsliso
        end interface
