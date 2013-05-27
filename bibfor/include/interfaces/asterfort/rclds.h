        interface
          subroutine rclds(a,nmax,nordre,x,nves)
            integer :: nves
            integer :: nordre
            integer :: nmax
            real(kind=8) :: a(nmax,nordre)
            complex(kind=8) :: x(nmax,nves)
          end subroutine rclds
        end interface
