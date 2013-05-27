        interface
          subroutine cmqutr(basz,nomain,nomaou,nbma,nummai,prefix,&
     &ndinit)
            character(*) :: basz
            character(len=8) :: nomain
            character(len=8) :: nomaou
            integer :: nbma
            integer :: nummai(*)
            character(len=8) :: prefix
            integer :: ndinit
          end subroutine cmqutr
        end interface
