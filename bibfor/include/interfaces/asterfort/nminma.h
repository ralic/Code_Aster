        interface
          subroutine nminma(fonact,lischa,sddyna,solveu,numedd,numfix,&
     &meelem,measse)
            integer :: fonact(*)
            character(len=19) :: lischa
            character(len=19) :: sddyna
            character(len=19) :: solveu
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
          end subroutine nminma
        end interface
