        interface
          subroutine dvout(lout,n,sx,idigit,ifmt)
            integer :: lout
            integer :: n
            real(kind=8) :: sx(*)
            integer :: idigit
            character(*) :: ifmt
          end subroutine dvout
        end interface
