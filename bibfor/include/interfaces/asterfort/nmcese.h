        interface
          subroutine nmcese(modele,numedd,mate,carele,comref,compor,&
     &lischa,carcri,fonact,sdstat,defico,iterat,sdnume,sdpilo,valinc,&
     &solalg,veelem,veasse,sdtime,offset,typsel,sddisc,licite,rho,eta,&
     &etaf,criter,ldccvg,pilcvg,matass)
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
            integer :: iterat
            character(len=19) :: sdnume
            character(len=19) :: sdpilo
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: sdtime
            real(kind=8) :: offset
            character(len=24) :: typsel
            character(len=19) :: sddisc
            integer :: licite(2)
            real(kind=8) :: rho
            real(kind=8) :: eta(2)
            real(kind=8) :: etaf
            real(kind=8) :: criter
            integer :: ldccvg
            integer :: pilcvg
            character(len=19) :: matass
          end subroutine nmcese
        end interface
