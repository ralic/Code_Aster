        interface
          subroutine nmctcd(modele,mate,carele,fonact,compor,carcri,&
     &sdtime,sddisc,sddyna,numins,valinc,solalg,lischa,comref,defico,&
     &resoco,resocu,numedd,parcon,veelem,veasse,measse)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            integer :: fonact(*)
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            integer :: numins
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: lischa
            character(len=24) :: comref
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: resocu
            character(len=24) :: numedd
            real(kind=8) :: parcon(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=19) :: measse(*)
          end subroutine nmctcd
        end interface
