        interface
          subroutine phi152(model,option,mate,phibar,ma,nu,num,nbmode,&
     &solvez,indice,tabad)
            character(len=2) :: model
            character(*) :: option
            character(*) :: mate
            character(*) :: phibar
            character(len=8) :: ma
            character(len=14) :: nu
            character(len=14) :: num
            integer :: nbmode
            character(*) :: solvez
            integer :: indice
            integer :: tabad(5)
          end subroutine phi152
        end interface
