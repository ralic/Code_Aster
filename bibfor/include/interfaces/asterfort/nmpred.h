        interface
          subroutine nmpred(modele,numedd,numfix,mate,carele,comref,&
     &compor,lischa,method,solveu,fonact,parmet,carcri,sdimpr,sdstat,&
     &sdtime,sddisc,sdnume,sderro,numins,valinc,solalg,matass,maprec,&
     &defico,resoco,resocu,sddyna,meelem,measse,veelem,veasse,lerrit)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=19) :: lischa
            character(len=16) :: method(*)
            character(len=19) :: solveu
            integer :: fonact(*)
            real(kind=8) :: parmet(*)
            character(len=24) :: carcri
            character(len=24) :: sdimpr
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            character(len=19) :: sdnume
            character(len=24) :: sderro
            integer :: numins
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: matass
            character(len=19) :: maprec
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: resocu
            character(len=19) :: sddyna
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            logical :: lerrit
          end subroutine nmpred
        end interface
