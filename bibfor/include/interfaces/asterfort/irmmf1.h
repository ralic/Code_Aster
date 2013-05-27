        interface
          subroutine irmmf1(fid,nomamd,typent,nbrent,nbgrou,nomgen,&
     &nufaen,nomast,prefix,typgeo,nomtyp,nmatyp,infmed,nivinf,ifm)
            integer :: nbrent
            integer :: fid
            character(*) :: nomamd
            integer :: typent
            integer :: nbgrou
            character(len=24) :: nomgen(*)
            integer :: nufaen(nbrent)
            character(len=8) :: nomast
            character(len=6) :: prefix
            integer :: typgeo(*)
            character(len=8) :: nomtyp(*)
            integer :: nmatyp(*)
            integer :: infmed
            integer :: nivinf
            integer :: ifm
          end subroutine irmmf1
        end interface
