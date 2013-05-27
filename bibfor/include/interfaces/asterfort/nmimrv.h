        interface
          subroutine nmimrv(sdimpr,fonact,iterat,fetite,relcoe,relite,&
     &eta)
            character(len=24) :: sdimpr
            integer :: fonact(*)
            integer :: iterat
            integer :: fetite
            real(kind=8) :: relcoe
            integer :: relite
            real(kind=8) :: eta
          end subroutine nmimrv
        end interface
