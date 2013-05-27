        interface
          subroutine diares(n,nevec,a,lda,tau,evec,ldevec,work)
            integer :: ldevec
            integer :: lda
            integer :: n
            integer :: nevec
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: tau(*)
            complex(kind=8) :: evec(ldevec,*)
            complex(kind=8) :: work(*)
          end subroutine diares
        end interface
