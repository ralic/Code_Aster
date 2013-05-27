        interface
          subroutine wprest(a,x,n,m,y)
            integer :: n
            real(kind=8) :: a(n,*)
            real(kind=8) :: x(*)
            integer :: m
            complex(kind=8) :: y(*)
          end subroutine wprest
        end interface
