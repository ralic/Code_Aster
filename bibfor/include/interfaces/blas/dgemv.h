        interface
          subroutine dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
            integer :: lda
            character(len=1) :: trans
            integer :: m
            integer :: n
            real(kind=8) :: alpha
            real(kind=8) :: a(lda,*)
            real(kind=8) :: x(*)
            integer :: incx
            real(kind=8) :: beta
            real(kind=8) :: y(*)
            integer :: incy
          end subroutine dgemv
        end interface
