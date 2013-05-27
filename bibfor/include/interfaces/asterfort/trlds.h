        interface
          subroutine trlds(a,nmax,nordre,ierr)
            integer :: nordre
            integer :: nmax
            real(kind=8) :: a(nmax,nordre)
            integer :: ierr
          end subroutine trlds
        end interface
