        interface
          subroutine flacon(n,v,x,isgn,est,kase)
            integer :: n
            real(kind=8) :: v(*)
            real(kind=8) :: x(*)
            integer :: isgn(*)
            real(kind=8) :: est
            integer :: kase
          end subroutine flacon
        end interface
