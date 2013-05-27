        interface
          subroutine nmassi(modele,numedd,lischa,fonact,sddyna,valinc,&
     &veelem,veasse,cndonn,matass)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=19) :: lischa
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=19) :: valinc(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=19) :: cndonn
            character(len=19) :: matass
          end subroutine nmassi
        end interface
