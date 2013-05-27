        interface
          subroutine cclodr(nuoplo,nbordr,lisord,nobase,minord,maxord,&
     &resuin,resuou,lacalc)
            integer :: nuoplo
            integer :: nbordr
            character(len=19) :: lisord
            character(len=8) :: nobase
            integer :: minord
            integer :: maxord
            character(len=8) :: resuin
            character(len=8) :: resuou
            character(len=24) :: lacalc
          end subroutine cclodr
        end interface
