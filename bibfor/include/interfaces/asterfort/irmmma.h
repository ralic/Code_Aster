        interface
          subroutine irmmma(fid,nomamd,nbmail,connex,point,typma,&
     &nommai,prefix,nbtyp,typgeo,nomtyp,nnotyp,renumd,nmatyp,infmed,&
     &modnum,nuanom)
            integer :: fid
            character(*) :: nomamd
            integer :: nbmail
            integer :: connex(*)
            integer :: point(*)
            integer :: typma(*)
            character(len=8) :: nommai(*)
            character(len=6) :: prefix
            integer :: nbtyp
            integer :: typgeo(*)
            character(len=8) :: nomtyp(*)
            integer :: nnotyp(*)
            integer :: renumd(*)
            integer :: nmatyp(*)
            integer :: infmed
            integer :: modnum(69)
            integer :: nuanom(69,*)
          end subroutine irmmma
        end interface
