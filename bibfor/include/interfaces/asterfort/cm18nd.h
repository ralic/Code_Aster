        interface
          subroutine cm18nd(nbno,nbnomi,prefix,ndinit,nomipe,nomnoe,&
     &coor)
            integer :: nbnomi
            integer :: nbno
            character(len=8) :: prefix
            integer :: ndinit
            integer :: nomipe(8,nbnomi)
            character(len=24) :: nomnoe
            real(kind=8) :: coor(3,*)
          end subroutine cm18nd
        end interface
