        interface
          subroutine dsortc(which,apply,n,xreal,ximag,y)
            integer :: n
            character(len=2) :: which
            logical :: apply
            real(kind=8) :: xreal(0:n-1)
            real(kind=8) :: ximag(0:n-1)
            real(kind=8) :: y(0:n-1)
          end subroutine dsortc
        end interface
