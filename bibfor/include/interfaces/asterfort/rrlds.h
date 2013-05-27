        interface
          subroutine rrlds(a,nmax,nordre,x,nves)
            integer :: nves
            integer :: nordre
            integer :: nmax
            real(kind=8) :: a(nmax,nordre)
            real(kind=8) :: x(nmax,nves)
          end subroutine rrlds
        end interface
