        interface
          subroutine ndnpas(fonact,numedd,numins,sddisc,sddyna,scotch,&
     &valinc,solalg)
            integer :: fonact(*)
            character(len=24) :: numedd
            integer :: numins
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            logical :: scotch
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
          end subroutine ndnpas
        end interface
