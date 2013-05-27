        interface
          subroutine fmprhm(nbfonc,nbptot,sigm,rphmax)
            integer :: nbptot
            integer :: nbfonc
            real(kind=8) :: sigm(nbfonc*nbptot)
            real(kind=8) :: rphmax
          end subroutine fmprhm
        end interface
