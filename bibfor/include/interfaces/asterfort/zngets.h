        interface
          subroutine zngets(ishift,which,kev,np,ritz,bounds)
            integer :: np
            integer :: kev
            integer :: ishift
            character(len=2) :: which
            complex(kind=8) :: ritz(kev+np)
            complex(kind=8) :: bounds(kev+np)
          end subroutine zngets
        end interface
