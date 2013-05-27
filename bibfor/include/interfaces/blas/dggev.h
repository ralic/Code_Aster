        interface
          subroutine dggev(jobvl,jobvr,n,a,lda,b,ldb,alphar,alphai,&
     &beta,vl,ldvl,vr,ldvr,work,lwork,info)
            integer :: ldvr
            integer :: ldvl
            integer :: ldb
            integer :: lda
            character(len=1) :: jobvl
            character(len=1) :: jobvr
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: b(ldb,*)
            real(kind=8) :: alphar(*)
            real(kind=8) :: alphai(*)
            real(kind=8) :: beta(*)
            real(kind=8) :: vl(ldvl,*)
            real(kind=8) :: vr(ldvr,*)
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: info
          end subroutine dggev
        end interface
