        interface
          subroutine resmod(bmodal,nbmode,neq,numgen,mdgene,noecho,&
     &modsst)
            integer :: neq
            integer :: nbmode
            real(kind=8) :: bmodal(neq,*)
            character(len=24) :: numgen
            character(len=24) :: mdgene
            character(len=8) :: noecho(3)
            real(kind=8) :: modsst(nbmode,6)
          end subroutine resmod
        end interface
