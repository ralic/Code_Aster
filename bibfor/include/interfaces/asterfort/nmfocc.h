        interface
          subroutine nmfocc(phase,modele,mate,numedd,fonact,defico,&
     &resoco,sdstat,sdtime,solalg,valinc,veelem,veasse)
            character(len=10) :: phase
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: numedd
            integer :: fonact(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: solalg(*)
            character(len=19) :: valinc(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
          end subroutine nmfocc
        end interface
