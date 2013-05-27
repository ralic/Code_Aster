        interface
          subroutine vtmv(n,v,a,r)
            integer :: n
            real(kind=8) :: v(n)
            real(kind=8) :: a(n,n)
            real(kind=8) :: r
          end subroutine vtmv
        end interface
