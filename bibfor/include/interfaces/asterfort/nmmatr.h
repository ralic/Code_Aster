        interface
          subroutine nmmatr(phasez,fonact,lischa,solveu,numedd,sddyna,&
     &numins,defico,resoco,meelem,measse,matass)
            character(*) :: phasez
            integer :: fonact(*)
            character(len=19) :: lischa
            character(len=19) :: solveu
            character(len=24) :: numedd
            character(len=19) :: sddyna
            integer :: numins
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: matass
          end subroutine nmmatr
        end interface
