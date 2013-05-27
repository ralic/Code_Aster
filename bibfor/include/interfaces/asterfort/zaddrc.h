        interface
          subroutine zaddrc(m,n,alpha,x,incx,y,incy,a,lda)
            integer :: m
            integer :: n
            complex(kind=8) :: alpha
            complex(kind=8) :: x(*)
            integer :: incx
            complex(kind=8) :: y(*)
            integer :: incy
            complex(kind=8) :: a(*)
            integer :: lda
          end subroutine zaddrc
        end interface
