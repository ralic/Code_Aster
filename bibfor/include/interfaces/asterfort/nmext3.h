        interface
          subroutine nmext3(noma,champ,nomcha,nomchs,nbcmp,nbma,nbpi,&
     &nbspi,extrga,extrch,extrcp,listma,listpi,listsp,listcp,chgaus,&
     &chelga)
            character(len=8) :: noma
            character(len=19) :: champ
            character(len=24) :: nomcha
            character(len=24) :: nomchs
            integer :: nbcmp
            integer :: nbma
            integer :: nbpi
            integer :: nbspi
            character(len=8) :: extrga
            character(len=8) :: extrch
            character(len=8) :: extrcp
            character(len=24) :: listma
            character(len=24) :: listpi
            character(len=24) :: listsp
            character(len=24) :: listcp
            character(len=19) :: chgaus
            character(len=19) :: chelga
          end subroutine nmext3
        end interface
