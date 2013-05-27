        interface
          subroutine wpordo(type,shift,vpr,vpi,x,m,neq)
            integer :: neq
            integer :: m
            integer :: type
            complex(kind=8) :: shift
            real(kind=8) :: vpr(*)
            real(kind=8) :: vpi(*)
            complex(kind=8) :: x(neq,m)
          end subroutine wpordo
        end interface
