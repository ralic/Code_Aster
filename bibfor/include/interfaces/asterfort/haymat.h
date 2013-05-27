        interface
          subroutine haymat(fami,kpg,ksp,mod,imat,nmat,poum,coefel,&
     &coefpl,nvi,nr)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: mod
            integer :: imat
            character(*) :: poum
            real(kind=8) :: coefel(nmat)
            real(kind=8) :: coefpl(nmat)
            integer :: nvi
            integer :: nr
          end subroutine haymat
        end interface
