        interface
          subroutine betmat(fami,kpg,ksp,mod,imat,nmat,tempd,tempf,&
     &materd,materf,matcst,ndt,ndi,nr,nvi)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: tempd
            real(kind=8) :: tempf
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            integer :: ndt
            integer :: ndi
            integer :: nr
            integer :: nvi
          end subroutine betmat
        end interface
