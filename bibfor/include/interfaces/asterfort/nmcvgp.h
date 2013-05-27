        interface
          subroutine nmcvgp(sddisc,numins,sderro,valinc,fonact,defico,&
     &resoco)
            character(len=19) :: sddisc
            integer :: numins
            character(len=24) :: sderro
            character(len=19) :: valinc(*)
            integer :: fonact(*)
            character(len=24) :: defico
            character(len=24) :: resoco
          end subroutine nmcvgp
        end interface
