        interface
          subroutine gmlelt(igmsh,maxnod,nbtyma,nbmail,nbnoma,nuconn,&
     &versio)
            integer :: nbtyma
            integer :: igmsh
            integer :: maxnod
            integer :: nbmail
            integer :: nbnoma(nbtyma)
            integer :: nuconn(19,32)
            integer :: versio
          end subroutine gmlelt
        end interface
