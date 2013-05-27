        interface
          subroutine lrmmma(fid,nomamd,nbmail,nbnoma,nbtyp,typgeo,&
     &nomtyp,nnotyp,renumd,nmatyp,nommai,connex,typmai,prefix,infmed,&
     &modnum,numnoa)
            integer :: fid
            character(*) :: nomamd
            integer :: nbmail
            integer :: nbnoma
            integer :: nbtyp
            integer :: typgeo(69)
            character(len=8) :: nomtyp(*)
            integer :: nnotyp(69)
            integer :: renumd(*)
            integer :: nmatyp(69)
            character(len=24) :: nommai
            character(len=24) :: connex
            character(len=24) :: typmai
            character(len=6) :: prefix
            integer :: infmed
            integer :: modnum(69)
            integer :: numnoa(69,*)
          end subroutine lrmmma
        end interface
