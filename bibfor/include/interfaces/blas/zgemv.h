        interface
          subroutine zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
            integer :: lda
            character(len=1) :: trans
            integer :: m
            integer :: n
            complex(kind=8) :: alpha
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: x(*)
            integer :: incx
            complex(kind=8) :: beta
            complex(kind=8) :: y(*)
            integer :: incy
          end subroutine zgemv
        end interface
