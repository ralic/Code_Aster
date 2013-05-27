        interface
          subroutine nmchht(modele,numedd,mate,compor,carele,lischa,&
     &carcri,comref,fonact,sdstat,sddyna,sdtime,defico,resoco,resocu,&
     &valinc,sddisc,parcon,solalg,veasse,sdnume)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: mate
            character(len=24) :: compor
            character(len=24) :: carele
            character(len=19) :: lischa
            character(len=24) :: carcri
            character(len=24) :: comref
            integer :: fonact(*)
            character(len=24) :: sdstat
            character(len=19) :: sddyna
            character(len=24) :: sdtime
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: resocu
            character(len=19) :: valinc(*)
            character(len=19) :: sddisc
            real(kind=8) :: parcon(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veasse(*)
            character(len=19) :: sdnume
          end subroutine nmchht
        end interface
