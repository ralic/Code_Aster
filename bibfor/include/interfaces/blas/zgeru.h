        interface
          subroutine zgeru(m,n,alpha,x,incx,y,incy,a,lda)
            integer :: lda
            integer :: m
            integer :: n
            complex(kind=8) :: alpha
            complex(kind=8) :: x(*)
            integer :: incx
            complex(kind=8) :: y(*)
            integer :: incy
            complex(kind=8) :: a(lda,*)
          end subroutine zgeru
        end interface
