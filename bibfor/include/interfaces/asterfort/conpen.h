        interface
          subroutine conpen(macor,nbcor,macoc,nbcoc,lface,locorr,&
     &loreor,ma)
            integer :: nbcoc
            integer :: nbcor
            character(len=8) :: macor(nbcor+2)
            character(len=8) :: macoc(nbcoc+2)
            logical :: lface
            logical :: locorr
            logical :: loreor
            character(len=8) :: ma
          end subroutine conpen
        end interface
