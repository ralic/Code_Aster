        interface
          subroutine phi199(model,mate,ma,nu,num,nbmode,solvez,indice,&
     &tabad)
            character(len=2) :: model
            character(*) :: mate
            character(len=8) :: ma
            character(len=14) :: nu
            character(len=14) :: num
            integer :: nbmode
            character(*) :: solvez
            integer :: indice
            integer :: tabad(*)
          end subroutine phi199
        end interface
