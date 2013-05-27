        interface
          subroutine nmpost(modele,noma,numedd,numfix,carele,compor,&
     &solveu,numins,mate,comref,lischa,defico,resoco,resocu,parmet,&
     &parcon,fonact,carcri,sdimpr,sdstat,sddisc,sdtime,sdobse,sderro,&
     &sdieto,sddyna,sdpost,valinc,solalg,meelem,measse,veelem,veasse,&
     &sdener,sdcriq,eta)
            character(len=24) :: modele
            character(len=8) :: noma
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
            character(len=24) :: resocu
            real(kind=8) :: parmet(*)
            real(kind=8) :: parcon(*)
            integer :: fonact(*)
            character(len=24) :: carcri
            character(len=24) :: sdimpr
            character(len=24) :: sdstat
            character(len=19) :: sddisc
            character(len=24) :: sdtime
            character(len=19) :: sdobse
            character(len=24) :: sderro
            character(len=24) :: sdieto
            character(len=19) :: sddyna
            character(len=19) :: sdpost
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=19) :: sdener
            character(len=24) :: sdcriq
            real(kind=8) :: eta
          end subroutine nmpost
        end interface
