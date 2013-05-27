        interface
          subroutine nmmajc(fonact,sddyna,sdnume,deltat,numedd,valinc,&
     &solalg)
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=19) :: sdnume
            real(kind=8) :: deltat
            character(len=24) :: numedd
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
          end subroutine nmmajc
        end interface
