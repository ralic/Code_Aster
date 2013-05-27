        interface
          subroutine rrldc(a,nordre,x,nves)
            integer :: nves
            integer :: nordre
            complex(kind=8) :: a(*)
            complex(kind=8) :: x(nordre,nves)
          end subroutine rrldc
        end interface
