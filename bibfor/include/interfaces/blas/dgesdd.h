        interface
          subroutine dgesdd(jobz,m,n,a,lda,s,u,ldu,vt,ldvt,work,lwork,&
     &iwork,info)
            integer :: ldvt
            integer :: ldu
            integer :: lda
            character(len=1) :: jobz
            integer :: m
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: s(*)
            real(kind=8) :: u(ldu,*)
            real(kind=8) :: vt(ldvt,*)
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: iwork(*)
            integer :: info
          end subroutine dgesdd
        end interface
