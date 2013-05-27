        interface
          subroutine zggevx(balanc,jobvl,jobvr,sense,n,a,lda,b,ldb,&
     &alpha,beta,vl,ldvl,vr,ldvr,ilo,ihi,lscale,rscale,abnrm,bbnrm,&
     &rconde,rcondv,work,lwork,rwork,iwork,bwork,info)
            integer :: ldvr
            integer :: ldvl
            integer :: ldb
            integer :: lda
            character(len=1) :: balanc
            character(len=1) :: jobvl
            character(len=1) :: jobvr
            character(len=1) :: sense
            integer :: n
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: b(ldb,*)
            complex(kind=8) :: alpha(*)
            complex(kind=8) :: beta(*)
            complex(kind=8) :: vl(ldvl,*)
            complex(kind=8) :: vr(ldvr,*)
            integer :: ilo
            integer :: ihi
            real(kind=8) :: lscale(*)
            real(kind=8) :: rscale(*)
            real(kind=8) :: abnrm
            real(kind=8) :: bbnrm
            real(kind=8) :: rconde(*)
            real(kind=8) :: rcondv(*)
            complex(kind=8) :: work(*)
            integer :: lwork
            real(kind=8) :: rwork(*)
            integer :: iwork(*)
            logical :: bwork(*)
            integer :: info
          end subroutine zggevx
        end interface
