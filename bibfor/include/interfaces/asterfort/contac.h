        interface
          subroutine contac(macor,nbcor,macoc,nbcoc,lface,lomodi,&
     &locorr,loreor,ma)
            integer :: nbcoc
            integer :: nbcor
            character(len=8) :: macor(nbcor+2)
            character(len=8) :: macoc(nbcoc+2)
            logical :: lface
            logical :: lomodi
            logical :: locorr
            logical :: loreor
            character(len=8) :: ma
          end subroutine contac
        end interface
