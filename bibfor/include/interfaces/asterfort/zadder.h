        interface
          subroutine zadder(uplo,n,alpha,x,incx,a,lda)
            character(*) :: uplo
            integer :: n
            real(kind=8) :: alpha
            complex(kind=8) :: x(*)
            integer :: incx
            complex(kind=8) :: a(*)
            integer :: lda
          end subroutine zadder
        end interface
