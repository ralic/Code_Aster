        interface
          subroutine ndexpl(modele,numedd,numfix,mate,carele,comref,&
     &compor,lischa,method,fonact,carcri,parcon,sdimpr,sdstat,sdnume,&
     &sddyna,sddisc,sdtime,sderro,valinc,numins,solalg,solveu,matass,&
     &maprec,meelem,measse,veelem,veasse,nbiter)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=19) :: lischa
            character(len=16) :: method(*)
            integer :: fonact(*)
            character(len=24) :: carcri
            real(kind=8) :: parcon(*)
            character(len=24) :: sdimpr
            character(len=24) :: sdstat
            character(len=19) :: sdnume
            character(len=19) :: sddyna
            character(len=19) :: sddisc
            character(len=24) :: sdtime
            character(len=24) :: sderro
            character(len=19) :: valinc(*)
            integer :: numins
            character(len=19) :: solalg(*)
            character(len=19) :: solveu
            character(len=19) :: matass
            character(len=19) :: maprec
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            integer :: nbiter
          end subroutine ndexpl
        end interface
