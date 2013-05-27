        interface
          subroutine ircers(ifi,ligrel,nbgrel,longr,ncmpmx,vale,nomgd,&
     &nomcmp,titr,nomel,loc,celd,nbnoma,permut,maxnod,typma,nomsd,nomsym&
     &,ir,nbmat,nummai,lmasu,ncmpu,nucmp,nbcmp,ncmps,nocmpl)
            integer :: maxnod
            integer :: ifi
            integer :: ligrel(*)
            integer :: nbgrel
            integer :: longr(*)
            integer :: ncmpmx
            real(kind=8) :: vale(*)
            character(*) :: nomgd
            character(*) :: nomcmp(*)
            character(*) :: titr
            character(*) :: nomel(*)
            character(*) :: loc
            integer :: celd(*)
            integer :: nbnoma(*)
            integer :: permut(maxnod,*)
            integer :: typma(*)
            character(*) :: nomsd
            character(*) :: nomsym
            integer :: ir
            integer :: nbmat
            integer :: nummai(*)
            logical :: lmasu
            integer :: ncmpu
            integer :: nucmp(*)
            integer :: nbcmp
            integer :: ncmps(*)
            character(*) :: nocmpl(*)
          end subroutine ircers
        end interface
