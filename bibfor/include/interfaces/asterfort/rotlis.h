        interface
          subroutine rotlis(nomres,fmli,icar,fplin,fplio,ii,sst,intf,&
     &fact)
            character(len=8) :: nomres
            character(len=24) :: fmli
            integer :: icar(3)
            character(len=24) :: fplin
            character(len=24) :: fplio
            integer :: ii
            character(len=8) :: sst
            character(len=8) :: intf
            real(kind=8) :: fact
          end subroutine rotlis
        end interface
