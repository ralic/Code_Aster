        interface
          subroutine nmmass(fonact,lischa,sddyna,solveu,numedd,numfix,&
     &meelem,masse)
            integer :: fonact(*)
            character(len=19) :: lischa
            character(len=19) :: sddyna
            character(len=19) :: solveu
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=19) :: meelem(*)
            character(len=19) :: masse
          end subroutine nmmass
        end interface
