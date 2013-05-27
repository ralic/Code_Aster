        interface
          subroutine xcgfvo(option,ndim,nnop,fno,rho)
            integer :: nnop
            integer :: ndim
            character(len=16) :: option
            real(kind=8) :: fno(ndim*nnop)
            real(kind=8) :: rho
          end subroutine xcgfvo
        end interface
