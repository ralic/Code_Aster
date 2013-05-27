        interface
          subroutine dgeev(jobvl,jobvr,n,a,lda,wr,wi,vl,ldvl,vr,ldvr,&
     &work,lwork,info)
            integer :: ldvr
            integer :: ldvl
            integer :: lda
            character(len=1) :: jobvl
            character(len=1) :: jobvr
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: wr(*)
            real(kind=8) :: wi(*)
            real(kind=8) :: vl(ldvl,*)
            real(kind=8) :: vr(ldvr,*)
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: info
          end subroutine dgeev
        end interface
