        interface
          subroutine burafd(materd,materf,nmat,afd,bfd,cfd)
            integer :: nmat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: afd(6)
            real(kind=8) :: bfd(6,6)
            real(kind=8) :: cfd(6,6)
          end subroutine burafd
        end interface
