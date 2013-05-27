        interface
          subroutine amumps(action,kxmps,rsolu,vcine,nbsol,iret,impr,&
     &ifmump,prepos,pcentp)
            character(*) :: action
            integer :: kxmps
            real(kind=8) :: rsolu(*)
            character(len=19) :: vcine
            integer :: nbsol
            integer :: iret
            character(len=14) :: impr
            integer :: ifmump
            logical :: prepos
            integer :: pcentp(2)
          end subroutine amumps
        end interface
