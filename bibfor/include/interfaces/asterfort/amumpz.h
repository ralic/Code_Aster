        interface
          subroutine amumpz(action,kxmps,csolu,vcine,nbsol,iret,impr,&
     &ifmump,prepos,pcentp)
            character(*) :: action
            integer :: kxmps
            complex(kind=8) :: csolu(*)
            character(len=19) :: vcine
            integer :: nbsol
            integer :: iret
            character(len=14) :: impr
            integer :: ifmump
            logical :: prepos
            integer :: pcentp(2)
          end subroutine amumpz
        end interface
