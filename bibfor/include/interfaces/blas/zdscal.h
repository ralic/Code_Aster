        interface
          subroutine zdscal(n,da,zx,incx)
            integer :: n
            real(kind=8) :: da
            complex(kind=8) :: zx(*)
            integer :: incx
          end subroutine zdscal
        end interface
