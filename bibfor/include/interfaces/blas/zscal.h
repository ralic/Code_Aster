        interface
          subroutine zscal(n,za,zx,incx)
            integer :: n
            complex(kind=8) :: za
            complex(kind=8) :: zx(*)
            integer :: incx
          end subroutine zscal
        end interface
