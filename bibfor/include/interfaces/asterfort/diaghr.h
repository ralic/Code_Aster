        interface
          subroutine diaghr(n,a,lda,eval,evec,ldevec,acopy,rwk,cwk)
            integer :: ldevec
            integer :: lda
            integer :: n
            complex(kind=8) :: a(lda,*)
            real(kind=8) :: eval(*)
            complex(kind=8) :: evec(ldevec,*)
            complex(kind=8) :: acopy(n,*)
            real(kind=8) :: rwk(*)
            complex(kind=8) :: cwk(*)
          end subroutine diaghr
        end interface
