        interface
          subroutine dlacpy(uplo,m,n,a,lda,b,ldb)
            integer :: ldb
            integer :: lda
            character(len=1) :: uplo
            integer :: m
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: b(ldb,*)
          end subroutine dlacpy
        end interface
