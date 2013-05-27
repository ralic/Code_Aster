        interface
          subroutine cclpci(option,modele,resuin,resuou,mateco,carael,&
     &ligrel,numord,nbpain,lipain,lichin,codret)
            character(len=16) :: option
            character(len=8) :: modele
            character(len=8) :: resuin
            character(len=8) :: resuou
            character(len=8) :: mateco
            character(len=8) :: carael
            character(len=24) :: ligrel
            integer :: numord
            integer :: nbpain
            character(len=8) :: lipain(*)
            character(len=24) :: lichin(*)
            integer :: codret
          end subroutine cclpci
        end interface
