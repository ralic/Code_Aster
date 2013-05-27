        interface
          subroutine irrjac(fami,kpg,ksp,mod,nmat,mater,yf,dy,nmod,&
     &drdy)
            integer :: nmod
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: mod
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: yf(*)
            real(kind=8) :: dy(*)
            real(kind=8) :: drdy(nmod,nmod)
          end subroutine irrjac
        end interface
