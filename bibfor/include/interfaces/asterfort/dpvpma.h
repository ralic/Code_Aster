        interface
          subroutine dpvpma(mod,imat,nbmat,tempd,materd,materf,matcst,&
     &ndt,ndi,nvi,indal)
            integer :: nbmat
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: tempd
            real(kind=8) :: materd(nbmat,2)
            real(kind=8) :: materf(nbmat,2)
            character(len=3) :: matcst
            integer :: ndt
            integer :: ndi
            integer :: nvi
            integer :: indal
          end subroutine dpvpma
        end interface
