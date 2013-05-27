        interface
          subroutine nmevev(sddisc,numins,valinc,sderro,defico,resoco,&
     &nombcl)
            character(len=19) :: sddisc
            integer :: numins
            character(len=19) :: valinc(*)
            character(len=24) :: sderro
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=4) :: nombcl
          end subroutine nmevev
        end interface
