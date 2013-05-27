        interface
          subroutine mmmbca(noma,sddyna,iterat,defico,resoco,valinc,&
     &solalg,ctcsta,mmcvca)
            character(len=8) :: noma
            character(len=19) :: sddyna
            integer :: iterat
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            integer :: ctcsta
            logical :: mmcvca
          end subroutine mmmbca
        end interface
