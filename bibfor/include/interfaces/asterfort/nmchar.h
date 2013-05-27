        interface
          subroutine nmchar(mode,phasez,modele,numedd,mate,carele,&
     &compor,lischa,carcri,numins,sdtime,sddisc,parcon,fonact,resoco,&
     &resocu,comref,valinc,solalg,veelem,measse,veasse,sddyna)
            character(len=4) :: mode
            character(*) :: phasez
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=19) :: lischa
            character(len=24) :: carcri
            integer :: numins
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            real(kind=8) :: parcon(8)
            integer :: fonact(*)
            character(len=24) :: resoco
            character(len=24) :: resocu
            character(len=24) :: comref
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veasse(*)
            character(len=19) :: sddyna
          end subroutine nmchar
        end interface
