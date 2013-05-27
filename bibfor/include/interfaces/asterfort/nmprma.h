        interface
          subroutine nmprma(modelz,mate,carele,compor,carcri,parmet,&
     &method,lischa,numedd,numfix,solveu,comref,sdimpr,sdstat,sdtime,&
     &sddisc,sddyna,numins,fonact,defico,resoco,valinc,solalg,veelem,&
     &meelem,measse,maprec,matass,codere,faccvg,ldccvg)
            character(*) :: modelz
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=24) :: carcri
            real(kind=8) :: parmet(*)
            character(len=16) :: method(*)
            character(len=19) :: lischa
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=19) :: solveu
            character(len=24) :: comref
            character(len=24) :: sdimpr
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            integer :: numins
            integer :: fonact(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: maprec
            character(len=19) :: matass
            character(len=24) :: codere
            integer :: faccvg
            integer :: ldccvg
          end subroutine nmprma
        end interface
