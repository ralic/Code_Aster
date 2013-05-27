        interface
          subroutine nmdecp(sddisc,iterat,ievdac,typdec,nbrpas,deltac,&
     &ratio,optdec,ldcext,durdec,retdec)
            character(len=19) :: sddisc
            integer :: iterat
            integer :: ievdac
            character(len=4) :: typdec
            integer :: nbrpas
            real(kind=8) :: deltac
            real(kind=8) :: ratio
            character(len=16) :: optdec
            logical :: ldcext
            real(kind=8) :: durdec
            integer :: retdec
          end subroutine nmdecp
        end interface
