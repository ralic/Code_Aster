        interface
          subroutine nmpich(modele,numedd,mate,carele,comref,compor,&
     &lischa,carcri,fonact,sdstat,defico,resoco,sdpilo,iterat,sdnume,&
     &deltat,valinc,solalg,veelem,veasse,sdtime,sddisc,eta,rho,offset,&
     &ldccvg,pilcvg,matass)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=19) :: lischa
            character(len=24) :: carcri
            integer :: fonact(*)
            character(len=24) :: sdstat
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: sdpilo
            integer :: iterat
            character(len=19) :: sdnume
            real(kind=8) :: deltat
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            real(kind=8) :: eta
            real(kind=8) :: rho
            real(kind=8) :: offset
            integer :: ldccvg
            integer :: pilcvg
            character(len=19) :: matass
          end subroutine nmpich
        end interface
