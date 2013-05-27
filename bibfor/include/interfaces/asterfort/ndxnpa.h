        interface
          subroutine ndxnpa(modele,mate,carele,lischa,fonact,sdimpr,&
     &sddisc,sddyna,sdnume,numedd,numins,valinc,solalg)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: lischa
            integer :: fonact(*)
            character(len=24) :: sdimpr
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            character(len=19) :: sdnume
            character(len=24) :: numedd
            integer :: numins
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
          end subroutine ndxnpa
        end interface
