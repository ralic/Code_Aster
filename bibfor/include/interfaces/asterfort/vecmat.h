        interface
          subroutine vecmat(fami,kpg,ksp,mod,loi,jmat,nmat,materd,&
     &materf,matcst,typma,ndt,ndi,nr,nvi)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: mod
            character(len=16) :: loi
            integer :: jmat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            character(len=8) :: typma
            integer :: ndt
            integer :: ndi
            integer :: nr
            integer :: nvi
          end subroutine vecmat
        end interface
