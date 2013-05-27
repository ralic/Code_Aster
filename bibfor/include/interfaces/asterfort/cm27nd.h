        interface
          subroutine cm27nd(nbno,nbnomi,nbnohe,nbma,lima,typema,connez&
     &,prefix,ndinit,nomipe,nomnoe,coor)
            integer :: nbnomi
            integer :: nbno
            integer :: nbnohe
            integer :: nbma
            integer :: lima(*)
            integer :: typema(*)
            character(*) :: connez
            character(len=8) :: prefix
            integer :: ndinit
            integer :: nomipe(8,nbnomi)
            character(len=24) :: nomnoe
            real(kind=8) :: coor(3,*)
          end subroutine cm27nd
        end interface
