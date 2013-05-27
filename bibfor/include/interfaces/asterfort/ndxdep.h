        interface
          subroutine ndxdep(numedd,fonact,numins,sddisc,sddyna,sdnume,&
     &valinc,solalg,veasse)
            character(len=24) :: numedd
            integer :: fonact(*)
            integer :: numins
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            character(len=19) :: sdnume
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veasse(*)
          end subroutine ndxdep
        end interface
