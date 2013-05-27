        interface
          subroutine dgesvd(jobu,jobvt,m,n,a,lda,s,u,ldu,vt,ldvt,work,&
     &lwork,info)
            integer :: ldvt
            integer :: ldu
            integer :: lda
            character(len=1) :: jobu
            character(len=1) :: jobvt
            integer :: m
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: s(*)
            real(kind=8) :: u(ldu,*)
            real(kind=8) :: vt(ldvt,*)
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: info
          end subroutine dgesvd
        end interface
