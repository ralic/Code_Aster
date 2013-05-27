        interface
          subroutine mulmat(k,n,m,a,b,c)
            integer :: m
            integer :: n
            integer :: k
            real(kind=8) :: a(k,n)
            real(kind=8) :: b(n,m)
            real(kind=8) :: c(k,m)
          end subroutine mulmat
        end interface
