        interface
          subroutine tridia(n,a,lda,d,e,tau,w)
            integer :: lda
            integer :: n
            complex(kind=8) :: a(lda,*)
            real(kind=8) :: d(*)
            real(kind=8) :: e(*)
            complex(kind=8) :: tau(*)
            complex(kind=8) :: w(*)
          end subroutine tridia
        end interface
