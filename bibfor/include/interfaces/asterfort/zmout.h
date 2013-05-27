        interface
          subroutine zmout(lout,m,n,a,lda,idigit,ifmt)
            integer :: lda
            integer :: lout
            integer :: m
            integer :: n
            complex(kind=8) :: a(lda,*)
            integer :: idigit
            character(*) :: ifmt
          end subroutine zmout
        end interface
