        interface
          subroutine nmfini(sddyna,valinc,measse,modele,mate,carele,&
     &compor,carcri,sdtime,sddisc,numins,solalg,lischa,comref,resoco,&
     &resocu,numedd,parcon,veelem,veasse)
            character(len=19) :: sddyna
            character(len=19) :: valinc(*)
            character(len=19) :: measse(*)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            integer :: numins
            character(len=19) :: solalg(*)
            character(len=19) :: lischa
            character(len=24) :: comref
            character(len=24) :: resoco
            character(len=24) :: resocu
            character(len=24) :: numedd
            real(kind=8) :: parcon(8)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
          end subroutine nmfini
        end interface
