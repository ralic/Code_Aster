        interface
          subroutine nmcere(modele,numedd,mate,carele,comref,compor,&
     &lischa,carcri,fonact,sdstat,defico,iterat,sdnume,valinc,solalg,&
     &veelem,veasse,sdtime,offset,rho,eta,residu,ldccvg,matass)
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
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: sdtime
            real(kind=8) :: offset
            real(kind=8) :: rho
            real(kind=8) :: eta
            real(kind=8) :: residu
            integer :: ldccvg
            character(len=19) :: matass
          end subroutine nmcere
        end interface
