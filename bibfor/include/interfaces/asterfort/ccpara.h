        interface
          subroutine ccpara(option,modele,resuin,resuou,numord,nordm1,&
     &exitim,mateco,carael)
            character(len=16) :: option
            character(len=8) :: modele
            character(len=8) :: resuin
            character(len=8) :: resuou
            integer :: numord
            integer :: nordm1
            logical :: exitim
            character(len=8) :: mateco
            character(len=8) :: carael
          end subroutine ccpara
        end interface
