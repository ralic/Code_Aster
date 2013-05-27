        interface
          subroutine algocl(sdstat,defico,resoco,solveu,matass,noma,&
     &ctccvg,ctcfix)
            character(len=24) :: sdstat
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: solveu
            character(len=19) :: matass
            character(len=8) :: noma
            integer :: ctccvg
            logical :: ctcfix
          end subroutine algocl
        end interface
