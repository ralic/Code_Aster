        interface
          subroutine fgrain(pic,npic,itrv,ncyc,sigmin,sigmax)
            real(kind=8) :: pic(*)
            integer :: npic
            integer :: itrv(*)
            integer :: ncyc
            real(kind=8) :: sigmin(*)
            real(kind=8) :: sigmax(*)
          end subroutine fgrain
        end interface
