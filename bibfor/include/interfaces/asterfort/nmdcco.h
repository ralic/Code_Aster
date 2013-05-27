        interface
          subroutine nmdcco(sddisc,ievdac,typdec,nbrpas,deltac,ratio,&
     &optdec,retdec,ldcext,subdur)
            character(len=19) :: sddisc
            integer :: ievdac
            character(len=4) :: typdec
            integer :: nbrpas
            real(kind=8) :: deltac
            real(kind=8) :: ratio
            character(len=16) :: optdec
            integer :: retdec
            logical :: ldcext
            real(kind=8) :: subdur
          end subroutine nmdcco
        end interface
