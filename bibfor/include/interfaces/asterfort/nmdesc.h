        interface
          subroutine nmdesc(modele,numedd,numfix,mate,carele,comref,&
     &compor,lischa,resoco,method,solveu,parmet,carcri,fonact,numins,&
     &iterat,sddisc,sdimpr,sdstat,sdtime,sddyna,sdnume,sderro,matass,&
     &maprec,defico,valinc,solalg,meelem,measse,veasse,veelem,lerrit)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=19) :: lischa
            character(len=24) :: resoco
            character(len=16) :: method(*)
            character(len=19) :: solveu
            real(kind=8) :: parmet(*)
            character(len=24) :: carcri
            integer :: fonact(*)
            integer :: numins
            integer :: iterat
            character(len=19) :: sddisc
            character(len=24) :: sdimpr
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: sddyna
            character(len=19) :: sdnume
            character(len=24) :: sderro
            character(len=19) :: matass
            character(len=19) :: maprec
            character(len=24) :: defico
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veasse(*)
            character(len=19) :: veelem(*)
            logical :: lerrit
          end subroutine nmdesc
        end interface
