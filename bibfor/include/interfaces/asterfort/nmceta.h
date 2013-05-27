        interface
          subroutine nmceta(modele,numedd,mate,carele,comref,compor,&
     &lischa,carcri,fonact,sdstat,defico,sdpilo,iterat,sdnume,valinc,&
     &solalg,veelem,veasse,sdtime,sddisc,nbeffe,irecli,proeta,offset,rho&
     &,etaf,ldccvg,pilcvg,residu,matass)
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
            character(len=19) :: sdpilo
            integer :: iterat
            character(len=19) :: sdnume
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            integer :: nbeffe
            logical :: irecli
            real(kind=8) :: proeta(2)
            real(kind=8) :: offset
            real(kind=8) :: rho
            real(kind=8) :: etaf
            integer :: ldccvg
            integer :: pilcvg
            real(kind=8) :: residu
            character(len=19) :: matass
          end subroutine nmceta
        end interface
