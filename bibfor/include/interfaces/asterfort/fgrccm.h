        interface
          subroutine fgrccm(nbextr,ext,ncyc,sigmin,sigmax)
            integer :: nbextr
            real(kind=8) :: ext(*)
            integer :: ncyc
            real(kind=8) :: sigmin(*)
            real(kind=8) :: sigmax(*)
          end subroutine fgrccm
        end interface
