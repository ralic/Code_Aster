        interface
          subroutine nsassp(modele,numedd,lischa,fonact,sddyna,sdtime,&
     &valinc,veelem,veasse,cnpilo,cndonn,mate,carele,defico,matass)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=19) :: lischa
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=24) :: sdtime
            character(len=19) :: valinc(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=19) :: cnpilo
            character(len=19) :: cndonn
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: defico
            character(len=19) :: matass
          end subroutine nsassp
        end interface
