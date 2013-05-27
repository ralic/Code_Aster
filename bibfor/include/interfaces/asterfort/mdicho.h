        interface
          subroutine mdicho(nomres,nbstoc,temps,forcho,deploc,vitcho,&
     &nbchto,nbchoc,parcho,noecho)
            integer :: nbchto
            character(len=8) :: nomres
            integer :: nbstoc
            real(kind=8) :: temps(*)
            real(kind=8) :: forcho(*)
            real(kind=8) :: deploc(*)
            real(kind=8) :: vitcho(*)
            integer :: nbchoc
            real(kind=8) :: parcho(nbchto,*)
            character(len=8) :: noecho(nbchto,*)
          end subroutine mdicho
        end interface
