        interface
          subroutine nxdoet(modele,numedd,lreuse,lostat,sdieto,initpr,&
     &instin)
            character(len=24) :: modele
            character(len=24) :: numedd
            logical :: lreuse
            logical :: lostat
            character(len=24) :: sdieto
            integer :: initpr
            real(kind=8) :: instin
          end subroutine nxdoet
        end interface
