        interface
          subroutine fmcros(nbfonc,nbptot,sigm,rd0,rtau0,rcrit,rphmax,&
     &rtaua)
            integer :: nbptot
            integer :: nbfonc
            real(kind=8) :: sigm(nbfonc*nbptot)
            real(kind=8) :: rd0
            real(kind=8) :: rtau0
            real(kind=8) :: rcrit
            real(kind=8) :: rphmax
            real(kind=8) :: rtaua
          end subroutine fmcros
        end interface
