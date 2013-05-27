        interface
          subroutine cfinit(noma,fonact,defico,resoco,numins,sddyna,&
     &valinc)
            character(len=8) :: noma
            integer :: fonact(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: numins
            character(len=19) :: sddyna
            character(len=19) :: valinc(*)
          end subroutine cfinit
        end interface
