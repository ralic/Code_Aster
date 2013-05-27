        interface
          subroutine nmsui3(sdimpr,typcha,nbma,nbno,nbpi,nbspi,nbcmp,&
     &extrch,extrcp,extrga,listma,chnoeu,chelga,champ,isuiv)
            character(len=24) :: sdimpr
            character(len=4) :: typcha
            integer :: nbma
            integer :: nbno
            integer :: nbpi
            integer :: nbspi
            integer :: nbcmp
            character(len=8) :: extrch
            character(len=8) :: extrcp
            character(len=8) :: extrga
            character(len=24) :: listma
            character(len=19) :: chnoeu
            character(len=19) :: chelga
            character(len=19) :: champ
            integer :: isuiv
          end subroutine nmsui3
        end interface
