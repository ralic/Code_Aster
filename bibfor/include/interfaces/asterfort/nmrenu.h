        interface
          subroutine nmrenu(modelz,fonact,numedd,lischa,solveu,resoco,&
     &renume)
            character(*) :: modelz
            integer :: fonact(*)
            character(len=24) :: numedd
            character(len=19) :: lischa
            character(len=19) :: solveu
            character(len=24) :: resoco
            logical :: renume
          end subroutine nmrenu
        end interface
