        interface
          subroutine nmobs2(noma,sdobse,nomtab,instan,titobs,typcha,&
     &nomcha,nomchs,nbma,nbno,nbpi,nbspi,nbcmp,extrga,extrch,extrcp,&
     &listno,listma,listpi,listsp,listcp,champ,chnoeu,chelga,nobsef)
            character(len=8) :: noma
            character(len=19) :: sdobse
            character(len=19) :: nomtab
            real(kind=8) :: instan
            character(len=80) :: titobs
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
            character(len=19) :: champ
            character(len=19) :: chnoeu
            character(len=19) :: chelga
            integer :: nobsef
          end subroutine nmobs2
        end interface
