        interface
          subroutine fondpl(modele,mate,numedd,neq,chondp,nchond,&
     &vecond,veonde,vaonde,temps,foonde)
            integer :: nchond
            integer :: neq
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: numedd
            character(len=8) :: chondp(nchond)
            character(len=24) :: vecond
            character(len=24) :: veonde
            character(len=24) :: vaonde
            real(kind=8) :: temps
            real(kind=8) :: foonde(neq)
          end subroutine fondpl
        end interface
