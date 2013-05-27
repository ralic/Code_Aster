        interface
          subroutine dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,&
     &c,ldc)
            integer :: ldc
            integer :: ldb
            integer :: lda
            character(len=1) :: transa
            character(len=1) :: transb
            integer :: m
            integer :: n
            integer :: k
            real(kind=8) :: alpha
            real(kind=8) :: a(lda,*)
            real(kind=8) :: b(ldb,*)
            real(kind=8) :: beta
            real(kind=8) :: c(ldc,*)
          end subroutine dgemm
        end interface
