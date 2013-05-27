        interface
          subroutine pmevdr(sddisc,tabinc,liccvg,itemax,conver,actite)
            character(len=19) :: sddisc
            character(len=19) :: tabinc(*)
            integer :: liccvg(*)
            logical :: itemax
            logical :: conver
            integer :: actite
          end subroutine pmevdr
        end interface
