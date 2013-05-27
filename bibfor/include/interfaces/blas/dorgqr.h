        interface
          subroutine dorgqr(m,n,k,a,lda,tau,work,lwork,info)
            integer :: lda
            integer :: m
            integer :: n
            integer :: k
            real(kind=8) :: a(lda,*)
            real(kind=8) :: tau(*)
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: info
          end subroutine dorgqr
        end interface
