        interface
          subroutine accel0(modele,numedd,numfix,fonact,lischa,defico,&
     &resoco,maprec,solveu,valinc,sddyna,sdstat,sdtime,meelem,measse,&
     &veelem,veasse,solalg)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: numfix
            integer :: fonact(*)
            character(len=19) :: lischa
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: maprec
            character(len=19) :: solveu
            character(len=19) :: valinc(*)
            character(len=19) :: sddyna
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=19) :: solalg(*)
          end subroutine accel0
        end interface
