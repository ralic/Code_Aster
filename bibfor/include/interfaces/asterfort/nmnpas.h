        interface
          subroutine nmnpas(modele,noma,mate,carele,lischa,fonact,&
     &sdimpr,sddisc,sdsuiv,sddyna,sdnume,sdstat,sdtime,numedd,numins,&
     &conv,defico,resoco,valinc,solalg,solveu)
            character(len=24) :: modele
            character(len=8) :: noma
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: lischa
            integer :: fonact(*)
            character(len=24) :: sdimpr
            character(len=19) :: sddisc
            character(len=24) :: sdsuiv
            character(len=19) :: sddyna
            character(len=19) :: sdnume
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=24) :: numedd
            integer :: numins
            real(kind=8) :: conv(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: solveu
          end subroutine nmnpas
        end interface
