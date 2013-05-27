        interface
          subroutine cfalgo(noma,sdstat,resigr,iterat,defico,resoco,&
     &solveu,numedd,matass,ddepla,depdel,ctccvg,ctcfix)
            character(len=8) :: noma
            character(len=24) :: sdstat
            real(kind=8) :: resigr
            integer :: iterat
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: solveu
            character(len=14) :: numedd
            character(len=19) :: matass
            character(len=19) :: ddepla
            character(len=19) :: depdel
            integer :: ctccvg
            logical :: ctcfix
          end subroutine cfalgo
        end interface
