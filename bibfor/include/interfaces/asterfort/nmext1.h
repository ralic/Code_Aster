        interface
          subroutine nmext1(noma,champ,typcha,nomcha,nomchs,nbma,nbno,&
     &nbpi,nbspi,nbcmp,extrga,extrch,extrcp,listno,listma,listpi,listsp,&
     &listcp,chnoeu,chgaus,chelga)
            character(len=8) :: noma
            character(len=19) :: champ
            character(len=4) :: typcha
            character(len=24) :: nomcha
            character(len=24) :: nomchs
            integer :: nbma
            integer :: nbno
            integer :: nbpi
            integer :: nbspi
            integer :: nbcmp
            character(len=8) :: extrga
            character(len=8) :: extrch
            character(len=8) :: extrcp
            character(len=24) :: listno
            character(len=24) :: listma
            character(len=24) :: listpi
            character(len=24) :: listsp
            character(len=24) :: listcp
            character(len=19) :: chnoeu
            character(len=19) :: chgaus
            character(len=19) :: chelga
          end subroutine nmext1
        end interface
