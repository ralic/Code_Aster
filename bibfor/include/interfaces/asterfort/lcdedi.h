        interface
          subroutine lcdedi(fami,kpg,ksp,nmat,materd,materf,tempd,&
     &tempf,tref,depst,epsdt,depsm,epsdm)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: tempd
            real(kind=8) :: tempf
            real(kind=8) :: tref
            real(kind=8) :: depst(6)
            real(kind=8) :: epsdt(6)
            real(kind=8) :: depsm(6)
            real(kind=8) :: epsdm(6)
          end subroutine lcdedi
        end interface
