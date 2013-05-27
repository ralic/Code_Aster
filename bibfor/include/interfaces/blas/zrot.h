        interface
          subroutine zrot(n,cx,incx,cy,incy,c,s)
            integer :: n
            complex(kind=8) :: cx(*)
            integer :: incx
            complex(kind=8) :: cy(*)
            integer :: incy
            real(kind=8) :: c
            complex(kind=8) :: s
          end subroutine zrot
        end interface
