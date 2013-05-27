        interface
          subroutine zgehrd(n,ilo,ihi,a,lda,tau,work,lwork,info)
            integer :: lda
            integer :: n
            integer :: ilo
            integer :: ihi
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: tau(*)
            complex(kind=8) :: work(*)
            integer :: lwork
            integer :: info
          end subroutine zgehrd
        end interface
