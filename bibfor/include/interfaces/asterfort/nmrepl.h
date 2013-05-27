        interface
          subroutine nmrepl(modele,numedd,mate,carele,comref,compor,&
     &lischa,parmet,carcri,fonact,iterat,sdstat,sdpilo,sdnume,sddyna,&
     &method,defico,resoco,deltat,valinc,solalg,veelem,veasse,sdtime,&
     &sddisc,etan,conv,eta,rho,offset,ldccvg,pilcvg,matass)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=19) :: lischa
            real(kind=8) :: parmet(*)
            character(len=24) :: carcri
            integer :: fonact(*)
            integer :: iterat
            character(len=24) :: sdstat
            character(len=19) :: sdpilo
            character(len=19) :: sdnume
            character(len=19) :: sddyna
            character(len=16) :: method(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            real(kind=8) :: deltat
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            real(kind=8) :: etan
            real(kind=8) :: conv(*)
            real(kind=8) :: eta
            real(kind=8) :: rho
            real(kind=8) :: offset
            integer :: ldccvg
            integer :: pilcvg
            character(len=19) :: matass
          end subroutine nmrepl
        end interface
