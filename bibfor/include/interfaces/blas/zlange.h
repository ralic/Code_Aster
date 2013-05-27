        interface
          function zlange(norm,m,n,a,lda,work)
            integer :: lda
            character(len=1) :: norm
            integer :: m
            integer :: n
            complex(kind=8) :: a(lda,*)
            real(kind=8) :: work(*)
            real(kind=8) :: zlange
          end function zlange
        end interface
