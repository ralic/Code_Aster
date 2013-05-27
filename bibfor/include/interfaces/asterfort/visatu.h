        interface
          subroutine visatu(nbvari,vintp,advico,vicsat,sat)
            integer :: nbvari
            real(kind=8) :: vintp(nbvari)
            integer :: advico
            integer :: vicsat
            real(kind=8) :: sat
          end subroutine visatu
        end interface
