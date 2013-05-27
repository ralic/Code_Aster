        interface
          subroutine fetrex(option,idd,ni,vi,no,vo,irex)
            integer :: no
            integer :: ni
            integer :: option
            integer :: idd
            real(kind=8) :: vi(ni)
            real(kind=8) :: vo(no)
            integer :: irex
          end subroutine fetrex
        end interface
