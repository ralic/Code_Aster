        interface
          subroutine matect(materd,materf,nmat,macst)
            integer :: nmat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: macst
          end subroutine matect
        end interface
