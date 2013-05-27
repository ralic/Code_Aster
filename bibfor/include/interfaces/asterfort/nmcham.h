        interface
          subroutine nmcham(fami,kpg,ksp,imate,compor,matel,mat,nbvar,&
     &memo,visc,idelta,coef)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            character(len=16) :: compor(3)
            real(kind=8) :: matel(4)
            real(kind=8) :: mat(18)
            integer :: nbvar
            integer :: memo
            integer :: visc
            integer :: idelta
            real(kind=8) :: coef
          end subroutine nmcham
        end interface
