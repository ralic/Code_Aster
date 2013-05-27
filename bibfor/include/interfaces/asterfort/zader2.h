        interface
          subroutine zader2(uplo,n,alpha,x,incx,y,incy,a,lda)
            integer :: lda
            character(*) :: uplo
            integer :: n
            complex(kind=8) :: alpha
            complex(kind=8) :: x(*)
            integer :: incx
            complex(kind=8) :: y(*)
            integer :: incy
            complex(kind=8) :: a(lda,*)
          end subroutine zader2
        end interface
