        interface
          subroutine matnor(fami,kpg,ksp,imat,nmat,poum,coefel,coefpl,&
     &ndt,nvi,nr)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imat
            character(*) :: poum
            real(kind=8) :: coefel(nmat)
            real(kind=8) :: coefpl(nmat)
            integer :: ndt
            integer :: nvi
            integer :: nr
          end subroutine matnor
        end interface
