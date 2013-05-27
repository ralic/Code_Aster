        interface
          subroutine nmfcon(modele,numedd,mate,fonact,defico,resoco,&
     &sdstat,sdtime,valinc,solalg,veelem,veasse)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: mate
            integer :: fonact(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
          end subroutine nmfcon
        end interface
