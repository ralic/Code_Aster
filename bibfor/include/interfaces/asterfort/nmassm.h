        interface
          subroutine nmassm(fonact,lischa,solveu,numedd,numfix,typmat,&
     &optasz,meelem,matass)
            integer :: fonact(*)
            character(len=19) :: lischa
            character(len=19) :: solveu
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=6) :: typmat
            character(*) :: optasz
            character(len=19) :: meelem(8)
            character(len=19) :: matass
          end subroutine nmassm
        end interface
