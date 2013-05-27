        interface
          subroutine lcdehy(fami,kpg,ksp,nmat,materd,materf,depsm,&
     &epsdm)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: depsm(6)
            real(kind=8) :: epsdm(6)
          end subroutine lcdehy
        end interface
