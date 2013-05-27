        interface
          subroutine nmcadt(sddisc,iadapt,numins,valinc,dtp)
            character(len=19) :: sddisc
            integer :: iadapt
            integer :: numins
            character(len=19) :: valinc(*)
            real(kind=8) :: dtp
          end subroutine nmcadt
        end interface
