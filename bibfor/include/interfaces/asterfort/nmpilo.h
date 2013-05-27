        interface
          subroutine nmpilo(sdpilo,deltat,rho,solalg,veasse,modele,&
     &mate,compor,resoco,valinc,nbatte,numedd,nbeffe,eta,pilcvg,carele)
            integer :: nbatte
            character(len=19) :: sdpilo
            real(kind=8) :: deltat
            real(kind=8) :: rho
            character(len=19) :: solalg(*)
            character(len=19) :: veasse(*)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: compor
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=24) :: numedd
            integer :: nbeffe
            real(kind=8) :: eta(nbatte)
            integer :: pilcvg
            character(len=24) :: carele
          end subroutine nmpilo
        end interface
