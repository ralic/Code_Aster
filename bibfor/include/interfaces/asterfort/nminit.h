        interface
          subroutine nminit(result,modele,numedd,numfix,mate,compor,&
     &carele,parmet,lischa,maprec,solveu,carcri,numins,sdstat,sddisc,&
     &sdnume,defico,sdcrit,comref,fonact,parcon,parcri,method,lisch2,&
     &noma,sdpilo,sddyna,sdimpr,sdsuiv,sdobse,sdtime,sderro,sdpost,&
     &sdieto,sdener,sdconv,sdcriq,deficu,resocu,resoco,valinc,solalg,&
     &measse,veelem,meelem,veasse,codere)
            character(len=8) :: result
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=24) :: mate
            character(len=24) :: compor
            character(len=24) :: carele
            real(kind=8) :: parmet(*)
            character(len=19) :: lischa
            character(len=19) :: maprec
            character(len=19) :: solveu
            character(len=24) :: carcri
            integer :: numins
            character(len=24) :: sdstat
            character(len=19) :: sddisc
            character(len=19) :: sdnume
            character(len=24) :: defico
            character(len=19) :: sdcrit
            character(len=24) :: comref
            integer :: fonact(*)
            real(kind=8) :: parcon(*)
            real(kind=8) :: parcri(*)
            character(len=16) :: method(*)
            character(len=19) :: lisch2
            character(len=8) :: noma
            character(len=19) :: sdpilo
            character(len=19) :: sddyna
            character(len=24) :: sdimpr
            character(len=24) :: sdsuiv
            character(len=19) :: sdobse
            character(len=24) :: sdtime
            character(len=24) :: sderro
            character(len=19) :: sdpost
            character(len=24) :: sdieto
            character(len=19) :: sdener
            character(len=24) :: sdconv
            character(len=24) :: sdcriq
            character(len=24) :: deficu
            character(len=24) :: resocu
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            character(len=19) :: meelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: codere
          end subroutine nminit
        end interface
