        interface
          subroutine mlmatc(ni,nk,nj,a,b,c)
            integer :: nk
            integer :: ni
            integer :: nj
            complex(kind=8) :: a(ni,*)
            complex(kind=8) :: b(nk,*)
            complex(kind=8) :: c(ni,*)
          end subroutine mlmatc
        end interface
