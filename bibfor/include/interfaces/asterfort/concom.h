        interface
          subroutine concom(macor,nblir,macoc,nblic,nbnoco,nococ)
            integer :: nblic
            integer :: nblir
            character(len=8) :: macor(nblir+2)
            character(len=8) :: macoc(nblic+2)
            integer :: nbnoco
            integer :: nococ(*)
          end subroutine concom
        end interface
