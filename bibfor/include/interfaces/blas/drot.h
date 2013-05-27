        interface
          subroutine drot(n,dx,incx,dy,incy,c,s)
            integer :: n
            real(kind=8) :: dx(*)
            integer :: incx
            real(kind=8) :: dy(*)
            integer :: incy
            real(kind=8) :: c
            real(kind=8) :: s
          end subroutine drot
        end interface
