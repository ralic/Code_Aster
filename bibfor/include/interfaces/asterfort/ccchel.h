        interface
          subroutine ccchel(option,modele,resuin,resuou,numord,nordm1,&
     &mateco,carael,typesd,ligrel,exipou,exitim,lischa,nbchre,ioccur,&
     &suropt,basopt,resout)
            character(len=16) :: option
            character(len=8) :: modele
            character(len=8) :: resuin
            character(len=8) :: resuou
            integer :: numord
            integer :: nordm1
            character(len=24) :: mateco
            character(len=8) :: carael
            character(len=16) :: typesd
            character(len=24) :: ligrel
            logical :: exipou
            logical :: exitim
            character(len=19) :: lischa
            integer :: nbchre
            integer :: ioccur
            character(len=24) :: suropt
            character(len=1) :: basopt
            character(len=24) :: resout
          end subroutine ccchel
        end interface
