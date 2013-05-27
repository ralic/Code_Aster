        interface
          subroutine ccchuc(resuin,resuou,chin,nchout,crit,nf,nfor,&
     &lisord,nbordr)
            integer :: nf
            character(len=8) :: resuin
            character(len=8) :: resuou
            character(len=16) :: chin
            integer :: nchout
            character(len=16) :: crit
            character(len=8) :: nfor(nf)
            character(len=19) :: lisord
            integer :: nbordr
          end subroutine ccchuc
        end interface
