        interface
          subroutine nmnble(numins,modele,noma,numedd,sdstat,sdtime,&
     &sddyna,sddisc,fonact,defico,resoco,valinc,solalg)
            integer :: numins
            character(len=24) :: modele
            character(len=8) :: noma
            character(len=24) :: numedd
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: sddyna
            character(len=19) :: sddisc
            integer :: fonact(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
          end subroutine nmnble
        end interface
