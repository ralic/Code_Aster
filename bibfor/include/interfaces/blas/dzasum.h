        interface
          function dzasum(n,zx,incx)
            integer :: n
            complex(kind=8) :: zx(*)
            integer :: incx
            real(kind=8) :: dzasum
          end function dzasum
        end interface
