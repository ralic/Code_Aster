        interface
          subroutine zgeqr2(m,n,a,lda,tau,work,info)
            integer :: lda
            integer :: m
            integer :: n
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: tau(*)
            complex(kind=8) :: work(*)
            integer :: info
          end subroutine zgeqr2
        end interface
