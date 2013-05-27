        interface
          subroutine nmpila(numedd,sdpilo,isxfe,dtau,depdel,ddepl0,&
     &ddepl1,nbeffe,eta,pilcvg)
            character(len=24) :: numedd
            character(len=19) :: sdpilo
            logical :: isxfe
            real(kind=8) :: dtau
            character(len=19) :: depdel
            character(len=19) :: ddepl0
            character(len=19) :: ddepl1
            integer :: nbeffe
            real(kind=8) :: eta(2)
            integer :: pilcvg
          end subroutine nmpila
        end interface
