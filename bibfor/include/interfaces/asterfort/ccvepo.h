        interface
          subroutine ccvepo(modele,resuin,lischa,nbchar,typesd,nbchre,&
     &ioccur,suropt,ligrel,exipou)
            character(len=8) :: modele
            character(len=8) :: resuin
            character(len=19) :: lischa
            integer :: nbchar
            character(len=16) :: typesd
            integer :: nbchre
            integer :: ioccur
            character(len=24) :: suropt
            character(len=24) :: ligrel
            logical :: exipou
          end subroutine ccvepo
        end interface
