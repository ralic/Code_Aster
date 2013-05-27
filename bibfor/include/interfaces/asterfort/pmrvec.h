        interface
          subroutine pmrvec(cumul,n,m,a,x,y)
            integer :: m
            integer :: n
            character(*) :: cumul
            real(kind=8) :: a(n,m)
            real(kind=8) :: x(m)
            real(kind=8) :: y(n)
          end subroutine pmrvec
        end interface
