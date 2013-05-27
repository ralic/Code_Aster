        interface
          subroutine cmlqnd(nbno,nbnomi,prefix,ndinit,nomipe,nomnoe,&
     &coor)
            integer :: nbnomi
            integer :: nbno
            character(len=8) :: prefix
            integer :: ndinit
            integer :: nomipe(2,nbnomi)
            character(len=24) :: nomnoe
            real(kind=8) :: coor(3,*)
          end subroutine cmlqnd
        end interface
