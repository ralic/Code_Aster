        interface
          subroutine nmcofr(noma,depplu,depdel,ddepla,solveu,numedd,&
     &matass,defico,resoco,iterat,resigr,sdstat,sdtime,ctccvg)
            character(len=8) :: noma
            character(len=19) :: depplu
            character(len=19) :: depdel
            character(len=19) :: ddepla
            character(len=19) :: solveu
            character(len=14) :: numedd
            character(len=19) :: matass
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: iterat
            real(kind=8) :: resigr
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            integer :: ctccvg
          end subroutine nmcofr
        end interface
