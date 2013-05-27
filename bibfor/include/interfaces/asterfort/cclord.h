        interface
          subroutine cclord(nuoplo,nbordr,lisord,nobase,optdem,minord,&
     &maxord,resuin,resuou,lisout)
            integer :: nuoplo
            integer :: nbordr
            character(len=19) :: lisord
            character(len=8) :: nobase
            logical :: optdem
            integer :: minord
            integer :: maxord
            character(len=8) :: resuin
            character(len=8) :: resuou
            character(len=24) :: lisout
          end subroutine cclord
        end interface
