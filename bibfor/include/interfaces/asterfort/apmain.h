        interface
          subroutine apmain(action,kptsc,rsolu,vcine,istop,iret)
            character(*) :: action
            integer :: kptsc
            real(kind=8) :: rsolu(*)
            character(len=19) :: vcine
            integer :: istop
            integer :: iret
          end subroutine apmain
        end interface
