        interface
          subroutine ndxpre(modele,numedd,numfix,mate,carele,comref,&
     &compor,lischa,method,solveu,fonact,carcri,sddisc,sdstat,sdtime,&
     &numins,valinc,solalg,matass,maprec,sddyna,sderro,meelem,measse,&
     &veelem,veasse,lerrit)
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
            character(len=24) :: carcri
            character(len=19) :: sddisc
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            integer :: numins
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: matass
            character(len=19) :: maprec
            character(len=19) :: sddyna
            character(len=24) :: sderro
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            logical :: lerrit
          end subroutine ndxpre
        end interface
