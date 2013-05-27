        interface
          subroutine lglmat(mod,imat,nbmat,tempd,materd,materf,matcst,&
     &ndt,ndi,nr,nvi)
            integer :: nbmat
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: tempd
            real(kind=8) :: materd(nbmat,2)
            real(kind=8) :: materf(nbmat,2)
            character(len=3) :: matcst
            integer :: ndt
            integer :: ndi
            integer :: nr
            integer :: nvi
          end subroutine lglmat
        end interface
