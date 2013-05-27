        interface
          subroutine dggevx(balanc,jobvl,jobvr,sense,n,a,lda,b,ldb,&
     &alphar,alphai,beta,vl,ldvl,vr,ldvr,ilo,ihi,lscale,rscale,abnrm,&
     &bbnrm,rconde,rcondv,work,lwork,iwork,bwork,info)
            integer :: ldvr
            integer :: ldvl
            integer :: ldb
            integer :: lda
            character(len=1) :: balanc
            character(len=1) :: jobvl
            character(len=1) :: jobvr
            character(len=1) :: sense
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: b(ldb,*)
            real(kind=8) :: alphar(*)
            real(kind=8) :: alphai(*)
            real(kind=8) :: beta(*)
            real(kind=8) :: vl(ldvl,*)
            real(kind=8) :: vr(ldvr,*)
            integer :: ilo
            integer :: ihi
            real(kind=8) :: lscale(*)
            real(kind=8) :: rscale(*)
            real(kind=8) :: abnrm
            real(kind=8) :: bbnrm
            real(kind=8) :: rconde(*)
            real(kind=8) :: rcondv(*)
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: iwork(*)
            logical :: bwork(*)
            integer :: info
          end subroutine dggevx
        end interface
