        interface
          subroutine zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,&
     &c,ldc)
            integer :: ldc
            integer :: ldb
            integer :: lda
            character(len=1) :: transa
            character(len=1) :: transb
            integer :: m
            integer :: n
            integer :: k
            complex(kind=8) :: alpha
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: b(ldb,*)
            complex(kind=8) :: beta
            complex(kind=8) :: c(ldc,*)
          end subroutine zgemm
        end interface
