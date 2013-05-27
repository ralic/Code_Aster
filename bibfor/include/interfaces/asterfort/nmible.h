        interface
          subroutine nmible(modele,noma,defico,resoco,fonact,numins,&
     &niveau,numedd,sdstat,sdtime,sdimpr)
            character(len=24) :: modele
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: fonact(*)
            integer :: numins
            integer :: niveau
            character(len=24) :: numedd
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=24) :: sdimpr
          end subroutine nmible
        end interface
