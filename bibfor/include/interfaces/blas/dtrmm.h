        interface
          subroutine dtrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb&
     &)
            integer :: ldb
            integer :: lda
            character(len=1) :: side
            character(len=1) :: uplo
            character(len=1) :: transa
            character(len=1) :: diag
            integer :: m
            integer :: n
            real(kind=8) :: alpha
            real(kind=8) :: a(lda,*)
            real(kind=8) :: b(ldb,*)
          end subroutine dtrmm
        end interface
