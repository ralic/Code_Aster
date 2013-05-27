        interface
          subroutine nmprac(fonact,lischa,numedd,numfix,solveu,sddyna,&
     &sdstat,sdtime,defico,resoco,meelem,measse,maprec,matass,faccvg)
            integer :: fonact(*)
            character(len=19) :: lischa
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=19) :: solveu
            character(len=19) :: sddyna
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: maprec
            character(len=19) :: matass
            integer :: faccvg
          end subroutine nmprac
        end interface
