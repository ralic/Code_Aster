        interface
          subroutine dgesvx(fact,trans,n,nrhs,a,lda,af,ldaf,ipiv,equed&
     &,r,c,b,ldb,x,ldx,rcond,ferr,berr,work,iwork,info)
            integer :: ldx
            integer :: ldb
            integer :: ldaf
            integer :: lda
            character(len=1) :: fact
            character(len=1) :: trans
            integer :: n
            integer :: nrhs
            real(kind=8) :: a(lda,*)
            real(kind=8) :: af(ldaf,*)
            integer :: ipiv(*)
            character(len=1) :: equed
            real(kind=8) :: r(*)
            real(kind=8) :: c(*)
            real(kind=8) :: b(ldb,*)
            real(kind=8) :: x(ldx,*)
            real(kind=8) :: rcond
            real(kind=8) :: ferr(*)
            real(kind=8) :: berr(*)
            real(kind=8) :: work(*)
            integer :: iwork(*)
            integer :: info
          end subroutine dgesvx
        end interface
