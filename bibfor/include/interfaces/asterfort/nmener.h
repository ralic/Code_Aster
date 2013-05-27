        interface
          subroutine nmener(valinc,veasse,measse,sddyna,eta,sdener,&
     &fonact,solveu,numedd,numfix,meelem,numins,modele,mate,carele,&
     &compor,carcri,sdtime,sddisc,solalg,lischa,comref,resoco,resocu,&
     &parcon,veelem)
            character(len=19) :: valinc(*)
            character(len=19) :: veasse(*)
            character(len=19) :: measse(*)
            character(len=19) :: sddyna
            real(kind=8) :: eta
            character(len=19) :: sdener
            integer :: fonact(*)
            character(len=19) :: solveu
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=19) :: meelem(*)
            integer :: numins
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            character(len=19) :: solalg(*)
            character(len=19) :: lischa
            character(len=24) :: comref
            character(len=24) :: resoco
            character(len=24) :: resocu
            real(kind=8) :: parcon(*)
            character(len=19) :: veelem(*)
          end subroutine nmener
        end interface
