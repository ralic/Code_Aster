        interface
          subroutine zlacpy(uplo,m,n,a,lda,b,ldb)
            integer :: ldb
            integer :: lda
            character(len=1) :: uplo
            integer :: m
            integer :: n
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: b(ldb,*)
          end subroutine zlacpy
        end interface
