        interface
          subroutine orient(mdgene,sst,jcoor,ino,coordo,itran)
            character(len=24) :: mdgene
            character(len=8) :: sst
            integer :: jcoor
            integer :: ino
            real(kind=8) :: coordo(3)
            integer :: itran
          end subroutine orient
        end interface
