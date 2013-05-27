        interface
          subroutine nmfpas(fonact,sddyna,sdpilo,sddisc,nbiter,numins,&
     &eta,valinc,solalg,veasse)
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=19) :: sdpilo
            character(len=19) :: sddisc
            integer :: nbiter
            integer :: numins
            real(kind=8) :: eta
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veasse(*)
          end subroutine nmfpas
        end interface
