        interface
          subroutine ccaccl(option,modele,resuin,mateco,carael,ligrel,&
     &typesd,nbpain,lipain,lichin,lichou,codret)
            character(len=16) :: option
            character(len=8) :: modele
            character(len=8) :: resuin
            character(len=8) :: mateco
            character(len=8) :: carael
            character(len=24) :: ligrel
            character(len=16) :: typesd
            integer :: nbpain
            character(len=8) :: lipain(*)
            character(len=24) :: lichin(*)
            character(len=24) :: lichou(2)
            integer :: codret
          end subroutine ccaccl
        end interface
