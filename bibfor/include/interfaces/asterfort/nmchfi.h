        interface
          subroutine nmchfi(parmet,method,fonact,sddisc,sddyna,numins,&
     &iterat,defico,lcfint,lcdiri,lcbudi,lcrigi,option)
            real(kind=8) :: parmet(*)
            character(len=16) :: method(*)
            integer :: fonact(*)
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            integer :: numins
            integer :: iterat
            character(len=24) :: defico
            logical :: lcfint
            logical :: lcdiri
            logical :: lcbudi
            logical :: lcrigi
            character(len=16) :: option
          end subroutine nmchfi
        end interface
