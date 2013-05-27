        interface
          subroutine burmat(fami,kpg,ksp,mod,imat,nmat,materd,materf,&
     &matcst,ndt,ndi,nr,nvi)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            integer :: ndt
            integer :: ndi
            integer :: nr
            integer :: nvi
          end subroutine burmat
        end interface
