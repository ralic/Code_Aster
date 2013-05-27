        interface
          subroutine ztrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb&
     &)
            integer :: ldb
            integer :: lda
            character(len=1) :: side
            character(len=1) :: uplo
            character(len=1) :: transa
            character(len=1) :: diag
            integer :: m
            integer :: n
            complex(kind=8) :: alpha
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: b(ldb,*)
          end subroutine ztrmm
        end interface
