        interface
          subroutine zunm2r(side,trans,m,n,k,a,lda,tau,c,ldc,work,info&
     &)
            integer :: ldc
            integer :: lda
            character(len=1) :: side
            character(len=1) :: trans
            integer :: m
            integer :: n
            integer :: k
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: tau(*)
            complex(kind=8) :: c(ldc,*)
            complex(kind=8) :: work(*)
            integer :: info
          end subroutine zunm2r
        end interface
