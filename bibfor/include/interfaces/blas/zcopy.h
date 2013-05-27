        interface
          subroutine zcopy(n,zx,incx,zy,incy)
            integer :: n
            complex(kind=8) :: zx(*)
            integer :: incx
            complex(kind=8) :: zy(*)
            integer :: incy
          end subroutine zcopy
        end interface
