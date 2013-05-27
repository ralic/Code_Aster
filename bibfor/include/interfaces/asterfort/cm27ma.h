        interface
          subroutine cm27ma(nbmato,nbma,nbno,nbnomi,lima,typema,conniz&
     &,connoz,nofils,nbtyma,nomast,reftyp,nbref,impmai)
            integer :: nbma
            integer :: nbmato
            integer :: nbno
            integer :: nbnomi
            integer :: lima(nbma)
            integer :: typema(*)
            character(*) :: conniz
            character(*) :: connoz
            integer :: nofils(6,*)
            integer :: nbtyma
            character(len=8) :: nomast(*)
            integer :: reftyp(*)
            integer :: nbref(*)
            integer :: impmai(*)
          end subroutine cm27ma
        end interface
