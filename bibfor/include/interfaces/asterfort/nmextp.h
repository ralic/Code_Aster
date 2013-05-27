        interface
          subroutine nmextp(motfac,iocc,nomcha,champ,nomchs,listpi,&
     &listsp,nbpi,nbspi,extrga)
            character(len=16) :: motfac
            integer :: iocc
            character(len=24) :: nomcha
            character(len=19) :: champ
            character(len=24) :: nomchs
            character(len=24) :: listpi
            character(len=24) :: listsp
            integer :: nbpi
            integer :: nbspi
            character(len=8) :: extrga
          end subroutine nmextp
        end interface
