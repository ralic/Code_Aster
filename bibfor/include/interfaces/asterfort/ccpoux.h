        interface
          subroutine ccpoux(resuin,typesd,nordre,nbchre,ioccur,kcharg,&
     &modele,nbpain,lipain,lichin,suropt,iret)
            character(len=8) :: resuin
            character(len=16) :: typesd
            integer :: nordre
            integer :: nbchre
            integer :: ioccur
            character(len=19) :: kcharg
            character(len=8) :: modele
            integer :: nbpain
            character(len=8) :: lipain(*)
            character(len=24) :: lichin(*)
            character(len=24) :: suropt
            integer :: iret
          end subroutine ccpoux
        end interface
