        interface
          subroutine nmevel(sddisc,numins,defico,resoco,vale,nombcl,&
     &lsvimx,ldvres,linsta,lerrcv,lerror,conver)
            character(len=19) :: sddisc
            integer :: numins
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: vale(*)
            character(len=4) :: nombcl
            logical :: lsvimx
            logical :: ldvres
            logical :: linsta
            logical :: lerrcv
            logical :: lerror
            logical :: conver
          end subroutine nmevel
        end interface
