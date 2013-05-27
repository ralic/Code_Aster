        interface
          subroutine nmchrm(phase,parmet,method,fonact,sddisc,sddyna,&
     &numins,iterat,defico,metpre,metcor,reasma)
            character(len=10) :: phase
            real(kind=8) :: parmet(*)
            character(len=16) :: method(*)
            integer :: fonact(*)
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            integer :: numins
            integer :: iterat
            character(len=24) :: defico
            character(len=16) :: metpre
            character(len=16) :: metcor
            logical :: reasma
          end subroutine nmchrm
        end interface
