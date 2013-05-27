        interface
          subroutine dscal(n,da,dx,incx)
            integer :: n
            real(kind=8) :: da
            real(kind=8) :: dx(*)
            integer :: incx
          end subroutine dscal
        end interface
