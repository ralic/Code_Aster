        interface
          subroutine lkimat(mod,imat,nmat,materd,materf,matcst,ndt,ndi&
     &,nvi,nr)
            integer :: nmat
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            integer :: ndt
            integer :: ndi
            integer :: nvi
            integer :: nr
          end subroutine lkimat
        end interface
