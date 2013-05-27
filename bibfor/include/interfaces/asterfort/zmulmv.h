        interface
          subroutine zmulmv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
            character(*) :: trans
            integer :: m
            integer :: n
            complex(kind=8) :: alpha
            complex(kind=8) :: a(*)
            integer :: lda
            complex(kind=8) :: x(*)
            integer :: incx
            complex(kind=8) :: beta
            complex(kind=8) :: y(*)
            integer :: incy
          end subroutine zmulmv
        end interface
