        interface
          subroutine xneuvi(narz,nar,nbno,tabdir,scorno,noeud,nliseq)
            integer :: nbno
            integer :: narz
            integer :: nar
            integer :: tabdir(narz,2)
            integer :: scorno(2*narz)
            integer :: noeud(2*narz)
            character(len=19) :: nliseq
          end subroutine xneuvi
        end interface
