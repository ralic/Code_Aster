        interface
          subroutine nmacex(sddisc,iterat,lextra,valext)
            character(len=19) :: sddisc
            integer :: iterat
            logical :: lextra
            real(kind=8) :: valext(4)
          end subroutine nmacex
        end interface
