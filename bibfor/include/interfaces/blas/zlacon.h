        interface
          subroutine zlacon(n,v,x,est,kase)
            integer :: n
            complex(kind=8) :: v(n)
            complex(kind=8) :: x(n)
            real(kind=8) :: est
            integer :: kase
          end subroutine zlacon
        end interface
