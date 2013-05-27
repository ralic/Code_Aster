        interface
          subroutine nmflma(typmat,mod45,defo,parmet,modelz,mate,&
     &carele,sddisc,sddyna,fonact,numins,valinc,solalg,lischa,comref,&
     &defico,resoco,solveu,numedd,numfix,compor,carcri,sdstat,sdtime,&
     &meelem,measse,veelem,nddle,ddlexc,modrig,ldccvg,matass,matgeo)
            character(len=16) :: typmat
            character(len=4) :: mod45
            integer :: defo
            real(kind=8) :: parmet(*)
            character(*) :: modelz
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            integer :: fonact(*)
            integer :: numins
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: lischa
            character(len=24) :: comref
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: solveu
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            integer :: nddle
            character(len=24) :: ddlexc
            character(len=16) :: modrig
            integer :: ldccvg
            character(len=19) :: matass
            character(len=19) :: matgeo
          end subroutine nmflma
        end interface
