        interface
          subroutine lecelt(iunv,maxnod,nbtyma,indic,permut,codgra,&
     &node,nbnode)
            integer :: nbtyma
            integer :: maxnod
            integer :: iunv
            integer :: indic(nbtyma)
            integer :: permut(maxnod,nbtyma)
            integer :: codgra
            integer :: node(maxnod)
            integer :: nbnode
          end subroutine lecelt
        end interface
