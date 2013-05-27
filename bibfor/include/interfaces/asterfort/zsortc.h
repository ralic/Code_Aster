        interface
          subroutine zsortc(which,apply,n,x,y)
            integer :: n
            character(len=2) :: which
            logical :: apply
            complex(kind=8) :: x(0:n-1)
            complex(kind=8) :: y(0:n-1)
          end subroutine zsortc
        end interface
