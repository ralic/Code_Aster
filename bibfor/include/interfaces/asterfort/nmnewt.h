        interface
          subroutine nmnewt(noma,modele,numins,numedd,numfix,mate,&
     &carele,comref,compor,lischa,method,fonact,carcri,parcon,conv,&
     &parmet,parcri,sdstat,sdieto,sdtime,sderro,sdimpr,sdnume,sddyna,&
     &sddisc,sdcrit,sdsuiv,sdpilo,sdconv,solveu,maprec,matass,valinc,&
     &solalg,meelem,measse,veelem,veasse,defico,resoco,deficu,resocu,eta&
     &,nbiter)
            character(len=8) :: noma
            character(len=24) :: modele
            integer :: numins
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
            real(kind=8) :: conv(*)
            real(kind=8) :: parmet(*)
            real(kind=8) :: parcri(*)
            character(len=24) :: sdstat
            character(len=24) :: sdieto
            character(len=24) :: sdtime
            character(len=24) :: sderro
            character(len=24) :: sdimpr
            character(len=19) :: sdnume
            character(len=19) :: sddyna
            character(len=19) :: sddisc
            character(len=19) :: sdcrit
            character(len=24) :: sdsuiv
            character(len=19) :: sdpilo
            character(len=24) :: sdconv
            character(len=19) :: solveu
            character(len=19) :: maprec
            character(len=19) :: matass
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: deficu
            character(len=24) :: resocu
            real(kind=8) :: eta
            integer :: nbiter
          end subroutine nmnewt
        end interface
