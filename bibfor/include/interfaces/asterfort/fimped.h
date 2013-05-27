        interface
          subroutine fimped(modele,mate,numedd,neq,vitini,vitent,&
     &veccor,veanec,vaanec,temps,foimpe)
            integer :: neq
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: numedd
            character(len=24) :: vitini
            character(len=24) :: vitent
            character(len=24) :: veccor
            character(len=24) :: veanec
            character(len=24) :: vaanec
            real(kind=8) :: temps
            real(kind=8) :: foimpe(neq)
          end subroutine fimped
        end interface
