        interface
          subroutine nmrelp(modele,numedd,mate,carele,comref,compor,&
     &lischa,carcri,fonact,iterat,sdstat,sdnume,sddyna,parmet,method,&
     &defico,valinc,solalg,veelem,veasse,sdtime,conv,ldccvg)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=19) :: lischa
            character(len=24) :: carcri
            integer :: fonact(*)
            integer :: iterat
            character(len=24) :: sdstat
            character(len=19) :: sdnume
            character(len=19) :: sddyna
            real(kind=8) :: parmet(*)
            character(len=16) :: method(*)
            character(len=24) :: defico
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: sdtime
            real(kind=8) :: conv(*)
            integer :: ldccvg
          end subroutine nmrelp
        end interface
