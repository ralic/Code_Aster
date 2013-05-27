        interface
          subroutine zaxpy(n,za,zx,incx,zy,incy)
            integer :: n
            complex(kind=8) :: za
            complex(kind=8) :: zx(*)
            integer :: incx
            complex(kind=8) :: zy(*)
            integer :: incy
          end subroutine zaxpy
        end interface
