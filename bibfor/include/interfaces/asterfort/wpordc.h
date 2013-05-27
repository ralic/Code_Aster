        interface
          subroutine wpordc(type,shift,vp,x,m,neq)
            integer :: neq
            integer :: m
            integer :: type
            complex(kind=8) :: shift
            complex(kind=8) :: vp(*)
            complex(kind=8) :: x(neq,m)
          end subroutine wpordc
        end interface
