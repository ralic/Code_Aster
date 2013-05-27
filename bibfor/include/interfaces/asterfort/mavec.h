        interface
          subroutine mavec(mp,m,mv,n)
            integer :: n
            integer :: m
            real(kind=8) :: mp(m,m)
            real(kind=8) :: mv(n)
          end subroutine mavec
        end interface
