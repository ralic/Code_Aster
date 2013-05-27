        interface
          subroutine ircecs(ifi,ligrel,nbgrel,longr,ncmpmx,vale,nomcmp&
     &,titr,nomel,loc,celd,nbnoma,permut,maxnod,typma,nomsd,nomsym,ir,&
     &nbmat,nummai,lmasu,ncmpu,nucmp)
            integer :: maxnod
            integer :: ifi
            integer :: ligrel(*)
            integer :: nbgrel
            integer :: longr(*)
            integer :: ncmpmx
            complex(kind=8) :: vale(*)
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
          end subroutine ircecs
        end interface
