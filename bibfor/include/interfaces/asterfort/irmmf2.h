        interface
          subroutine irmmf2(fid,nomamd,typent,nbrent,nbgrou,nomgen,&
     &nbec,nomast,prefix,typgeo,nomtyp,nmatyp,nufaen,nufacr,nogrfa,&
     &nofaex,tabaux,infmed,nivinf,ifm)
            integer :: nbgrou
            integer :: nbrent
            integer :: fid
            character(*) :: nomamd
            integer :: typent
            character(len=24) :: nomgen(*)
            integer :: nbec
            character(len=8) :: nomast
            character(len=6) :: prefix
            integer :: typgeo(*)
            character(len=8) :: nomtyp(*)
            integer :: nmatyp(*)
            integer :: nufaen(nbrent)
            integer :: nufacr(nbrent)
            character(len=80) :: nogrfa(nbgrou)
            character(*) :: nofaex(*)
            integer :: tabaux(*)
            integer :: infmed
            integer :: nivinf
            integer :: ifm
          end subroutine irmmf2
        end interface
