        interface
          subroutine daxpy(n,da,dx,incx,dy,incy)
            integer :: n
            real(kind=8) :: da
            real(kind=8) :: dx(*)
            integer :: incx
            real(kind=8) :: dy(*)
            integer :: incy
          end subroutine daxpy
        end interface
