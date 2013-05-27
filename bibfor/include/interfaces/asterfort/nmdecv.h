        interface
          subroutine nmdecv(sddisc,numins,ievdac,dtmin,retdec)
            character(len=19) :: sddisc
            integer :: numins
            integer :: ievdac
            real(kind=8) :: dtmin
            integer :: retdec
          end subroutine nmdecv
        end interface
