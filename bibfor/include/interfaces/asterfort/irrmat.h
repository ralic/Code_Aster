        interface
          subroutine irrmat(fami,kpg,ksp,model,imat,nmat,itmax,rela,&
     &materd,materf,matcst,ndt,ndi,nr,nvi)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: model
            integer :: imat
            integer :: itmax
            real(kind=8) :: rela
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            integer :: ndt
            integer :: ndi
            integer :: nr
            integer :: nvi
          end subroutine irrmat
        end interface
