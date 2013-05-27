        interface
          subroutine ndassp(modele,numedd,mate,carele,comref,compor,&
     &lischa,carcri,sdstat,fonact,defico,sddyna,valinc,solalg,veelem,&
     &veasse,sdtime,ldccvg,codere,cndonn,sdnume,matass)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=19) :: lischa
            character(len=24) :: carcri
            character(len=24) :: sdstat
            integer :: fonact(*)
            character(len=24) :: defico
            character(len=19) :: sddyna
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: sdtime
            integer :: ldccvg
            character(len=24) :: codere
            character(len=19) :: cndonn
            character(len=19) :: sdnume
            character(len=19) :: matass
          end subroutine ndassp
        end interface
