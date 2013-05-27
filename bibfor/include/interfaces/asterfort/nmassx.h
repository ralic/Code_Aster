        interface
          subroutine nmassx(modele,numedd,mate,carele,comref,compor,&
     &lischa,carcri,fonact,sdstat,sddyna,valinc,solalg,veelem,veasse,&
     &sdtime,ldccvg,codere,cndonn)
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=19) :: lischa
            character(len=24) :: carcri
            integer :: fonact(*)
            character(len=24) :: sdstat
            character(len=19) :: sddyna
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=24) :: sdtime
            integer :: ldccvg
            character(len=24) :: codere
            character(len=19) :: cndonn
          end subroutine nmassx
        end interface
