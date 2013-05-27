        interface
          subroutine nmassd(modele,numedd,lischa,fonact,depest,veasse,&
     &matass,cnpilo,cndonn)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=19) :: lischa
            integer :: fonact(*)
            character(len=19) :: depest
            character(len=19) :: veasse(*)
            character(len=19) :: matass
            character(len=19) :: cnpilo
            character(len=19) :: cndonn
          end subroutine nmassd
        end interface
