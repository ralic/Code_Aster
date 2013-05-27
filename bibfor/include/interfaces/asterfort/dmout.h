        interface
          subroutine dmout(lout,m,n,a,lda,idigit,ifmt)
            integer :: lda
            integer :: lout
            integer :: m
            integer :: n
            real(kind=8) :: a(lda,*)
            integer :: idigit
            character(*) :: ifmt
          end subroutine dmout
        end interface
