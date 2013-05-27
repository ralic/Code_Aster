        interface
          subroutine zvout(lout,n,cx,idigit,ifmt)
            integer :: lout
            integer :: n
            complex(kind=8) :: cx(*)
            integer :: idigit
            character(*) :: ifmt
          end subroutine zvout
        end interface
