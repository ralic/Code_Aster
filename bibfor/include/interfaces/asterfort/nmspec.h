        interface
          subroutine nmspec(modele,numedd,numfix,carele,compor,solveu,&
     &numins,mate,comref,lischa,defico,resoco,parmet,fonact,carcri,&
     &sdimpr,sdstat,sdtime,sddisc,valinc,solalg,meelem,measse,veelem,&
     &sddyna,sdpost,sderro)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=19) :: solveu
            integer :: numins
            character(len=24) :: mate
            character(len=24) :: comref
            character(len=19) :: lischa
            character(len=24) :: defico
            character(len=24) :: resoco
            real(kind=8) :: parmet(*)
            integer :: fonact(*)
            character(len=24) :: carcri
            character(len=24) :: sdimpr
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            character(len=19) :: sddyna
            character(len=19) :: sdpost
            character(len=24) :: sderro
          end subroutine nmspec
        end interface
