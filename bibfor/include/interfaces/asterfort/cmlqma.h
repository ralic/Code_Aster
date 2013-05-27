        interface
          subroutine cmlqma(nbmato,nbma,nbno,lima,typema,conniz,connoz&
     &,nofils,nbtyma,nomast,reftyp,nbref,impmai)
            integer :: nbma
            integer :: nbmato
            integer :: nbno
            integer :: lima(nbma)
            integer :: typema(*)
            character(*) :: conniz
            character(*) :: connoz
            integer :: nofils(12,*)
            integer :: nbtyma
            character(len=8) :: nomast(*)
            integer :: reftyp(*)
            integer :: nbref(*)
            integer :: impmai(*)
          end subroutine cmlqma
        end interface
