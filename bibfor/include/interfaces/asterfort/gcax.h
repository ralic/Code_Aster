        interface
          subroutine gcax(m,in,ip,ac,x,y)
            integer :: m
            integer :: in(m)
            integer(kind=4) :: ip(*)
            real(kind=8) :: ac(*)
            real(kind=8) :: x(m)
            real(kind=8) :: y(m)
          end subroutine gcax
        end interface
