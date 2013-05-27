        interface
          subroutine irmmfa(fid,nomamd,nbnoeu,nbmail,nomast,nbgrno,&
     &nomgno,nbgrma,nomgma,prefix,typgeo,nomtyp,nmatyp,infmed)
            integer :: fid
            character(*) :: nomamd
            integer :: nbnoeu
            integer :: nbmail
            character(len=8) :: nomast
            integer :: nbgrno
            character(len=24) :: nomgno(*)
            integer :: nbgrma
            character(len=24) :: nomgma(*)
            character(len=6) :: prefix
            integer :: typgeo(*)
            character(len=8) :: nomtyp(*)
            integer :: nmatyp(*)
            integer :: infmed
          end subroutine irmmfa
        end interface
