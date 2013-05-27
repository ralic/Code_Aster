        interface
          subroutine ndxmat(fonact,lischa,solveu,numedd,sddyna,numins,&
     &meelem,measse,matass)
            integer :: fonact(*)
            character(len=19) :: lischa
            character(len=19) :: solveu
            character(len=24) :: numedd
            character(len=19) :: sddyna
            integer :: numins
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: matass
          end subroutine ndxmat
        end interface
