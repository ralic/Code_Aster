        interface
          subroutine fgeqr2(m,n,a,lda,tau,work,info)
            integer :: lda
            integer :: m
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: tau(*)
            real(kind=8) :: work(*)
            integer :: info
          end subroutine fgeqr2
        end interface
