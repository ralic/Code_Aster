        interface
          subroutine genere(r,dim,v,x)
            integer :: dim
            complex(kind=8) :: r(dim,dim)
            complex(kind=8) :: v(dim)
            complex(kind=8) :: x(dim)
          end subroutine genere
        end interface
