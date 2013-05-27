        interface
          subroutine nmconv(noma,modele,mate,numedd,sdnume,fonact,&
     &sddyna,sdconv,sdimpr,sdstat,sddisc,sdtime,sdcrit,sderro,parmet,&
     &comref,matass,solveu,numins,iterat,conv,eta,parcri,defico,resoco,&
     &valinc,solalg,measse,veasse)
            character(len=8) :: noma
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: numedd
            character(len=19) :: sdnume
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=24) :: sdconv
            character(len=24) :: sdimpr
            character(len=24) :: sdstat
            character(len=19) :: sddisc
            character(len=24) :: sdtime
            character(len=19) :: sdcrit
            character(len=24) :: sderro
            real(kind=8) :: parmet(*)
            character(len=24) :: comref
            character(len=19) :: matass
            character(len=19) :: solveu
            integer :: numins
            integer :: iterat
            real(kind=8) :: conv(*)
            real(kind=8) :: eta
            real(kind=8) :: parcri(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: measse(*)
            character(len=19) :: veasse(*)
          end subroutine nmconv
        end interface
