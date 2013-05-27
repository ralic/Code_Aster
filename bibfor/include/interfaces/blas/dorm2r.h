        interface
          subroutine dorm2r(side,trans,m,n,k,a,lda,tau,c,ldc,work,info&
     &)
            integer :: ldc
            integer :: lda
            character(len=1) :: side
            character(len=1) :: trans
            integer :: m
            integer :: n
            integer :: k
            real(kind=8) :: a(lda,*)
            real(kind=8) :: tau(*)
            real(kind=8) :: c(ldc,*)
            real(kind=8) :: work(*)
            integer :: info
          end subroutine dorm2r
        end interface
