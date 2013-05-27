        interface
          subroutine nmpidd(numedd,sdpilo,dtau,depdel,ddepl0,ddepl1,&
     &eta,pilcvg,nbeffe)
            character(len=24) :: numedd
            character(len=19) :: sdpilo
            real(kind=8) :: dtau
            character(len=19) :: depdel
            character(len=19) :: ddepl0
            character(len=19) :: ddepl1
            real(kind=8) :: eta
            integer :: pilcvg
            integer :: nbeffe
          end subroutine nmpidd
        end interface
