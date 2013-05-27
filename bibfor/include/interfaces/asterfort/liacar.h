        interface
          subroutine liacar(nomres,sst,intf,fplin,fplio,ii,icar)
            character(len=8) :: nomres
            character(len=8) :: sst
            character(len=8) :: intf
            character(len=24) :: fplin
            character(len=24) :: fplio
            integer :: ii
            integer :: icar(3)
          end subroutine liacar
        end interface
