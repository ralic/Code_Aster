        interface
          subroutine zmvpy(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
            integer :: lda
            character(*) :: uplo
            integer :: n
            complex(kind=8) :: alpha
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: x(*)
            integer :: incx
            complex(kind=8) :: beta
            complex(kind=8) :: y(*)
            integer :: incy
          end subroutine zmvpy
        end interface
