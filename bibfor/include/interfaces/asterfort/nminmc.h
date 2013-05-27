        interface
          subroutine nminmc(fonact,lischa,sddyna,modele,compor,solveu,&
     &numedd,numfix,defico,resoco,carcri,solalg,valinc,mate,carele,&
     &sddisc,sdstat,sdtime,comref,meelem,measse,veelem,codere)
            integer :: fonact(*)
            character(len=19) :: lischa
            character(len=19) :: sddyna
            character(len=24) :: modele
            character(len=24) :: compor
            character(len=19) :: solveu
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: carcri
            character(len=19) :: solalg(*)
            character(len=19) :: valinc(*)
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: sddisc
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=24) :: comref
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            character(len=24) :: codere
          end subroutine nminmc
        end interface
