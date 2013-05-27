        interface
          subroutine matinv(stop,ndim,mat,inv,det)
            integer :: ndim
            character(len=1) :: stop
            real(kind=8) :: mat(ndim,ndim)
            real(kind=8) :: inv(ndim,ndim)
            real(kind=8) :: det
          end subroutine matinv
        end interface
