        interface
          subroutine dger(m,n,alpha,x,incx,y,incy,a,lda)
            integer :: lda
            integer :: m
            integer :: n
            real(kind=8) :: alpha
            real(kind=8) :: x(*)
            integer :: incx
            real(kind=8) :: y(*)
            integer :: incy
            real(kind=8) :: a(lda,*)
          end subroutine dger
        end interface
