        interface
          subroutine fmdevi(nbfonc,nbptot,sigm,dev)
            integer :: nbptot
            integer :: nbfonc
            real(kind=8) :: sigm(nbfonc*nbptot)
            real(kind=8) :: dev(nbfonc*nbptot)
          end subroutine fmdevi
        end interface
