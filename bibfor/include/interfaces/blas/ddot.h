        interface
          function ddot(n,dx,incx,dy,incy)
            integer :: n
            real(kind=8) :: dx(*)
            integer :: incx
            real(kind=8) :: dy(*)
            integer :: incy
            real(kind=8) :: ddot
          end function ddot
        end interface
