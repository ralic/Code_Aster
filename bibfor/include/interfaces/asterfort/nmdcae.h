        interface
          subroutine nmdcae(sddisc,iterat,typdec,nbrpas,ratio,optdec,&
     &retdec)
            character(len=19) :: sddisc
            integer :: iterat
            character(len=4) :: typdec
            integer :: nbrpas
            real(kind=8) :: ratio
            character(len=16) :: optdec
            integer :: retdec
          end subroutine nmdcae
        end interface
