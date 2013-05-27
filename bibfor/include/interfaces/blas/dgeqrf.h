        interface
          subroutine dgeqrf(m,n,a,lda,tau,work,lwork,info)
            integer :: lda
            integer :: m
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: tau(*)
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: info
          end subroutine dgeqrf
        end interface
