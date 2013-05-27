        interface
          subroutine zggev(jobvl,jobvr,n,a,lda,b,ldb,alpha,beta,vl,&
     &ldvl,vr,ldvr,work,lwork,rwork,info)
            integer :: ldvr
            integer :: ldvl
            integer :: ldb
            integer :: lda
            character(len=1) :: jobvl
            character(len=1) :: jobvr
            integer :: n
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: b(ldb,*)
            complex(kind=8) :: alpha(*)
            complex(kind=8) :: beta(*)
            complex(kind=8) :: vl(ldvl,*)
            complex(kind=8) :: vr(ldvr,*)
            complex(kind=8) :: work(*)
            integer :: lwork
            real(kind=8) :: rwork(*)
            integer :: info
          end subroutine zggev
        end interface
