        interface
          subroutine vecmap(mv,n,mp,m)
            integer :: m
            integer :: n
            real(kind=8) :: mv(n)
            real(kind=8) :: mp(m,m)
          end subroutine vecmap
        end interface
