        interface
          subroutine dcopy(n,dx,incx,dy,incy)
            integer :: n
            real(kind=8) :: dx(*)
            integer :: incx
            real(kind=8) :: dy(*)
            integer :: incy
          end subroutine dcopy
        end interface
