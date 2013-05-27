        interface
          subroutine pmat(n,a,b,c)
            integer :: n
            real(kind=8) :: a(n,n)
            real(kind=8) :: b(n,n)
            real(kind=8) :: c(n,n)
          end subroutine pmat
        end interface
