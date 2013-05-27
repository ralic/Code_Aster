        interface
          subroutine wptest(exclu,xh,xb,vp,neq,nmax)
            integer :: exclu(*)
            complex(kind=8) :: xh(*)
            complex(kind=8) :: xb(*)
            complex(kind=8) :: vp
            integer :: neq
            real(kind=8) :: nmax
          end subroutine wptest
        end interface
