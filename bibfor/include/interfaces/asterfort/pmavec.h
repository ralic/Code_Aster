        interface
          subroutine pmavec(cumul,n,a,x,y)
            integer :: n
            character(*) :: cumul
            real(kind=8) :: a(n,n)
            real(kind=8) :: x(n)
            real(kind=8) :: y(n)
          end subroutine pmavec
        end interface
