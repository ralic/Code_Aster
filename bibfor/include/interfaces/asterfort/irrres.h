        interface
          subroutine irrres(fami,kpg,ksp,mod,nmat,materd,materf,yd,yf,&
     &deps,dy,r)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: mod
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: yd(*)
            real(kind=8) :: yf(*)
            real(kind=8) :: deps(6)
            real(kind=8) :: dy(*)
            real(kind=8) :: r(*)
          end subroutine irrres
        end interface
