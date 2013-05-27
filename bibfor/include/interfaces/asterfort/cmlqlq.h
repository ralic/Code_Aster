        interface
          subroutine cmlqlq(main,maout,nbma,lima,prefix,ndinit)
            integer :: nbma
            character(len=8) :: main
            character(len=8) :: maout
            integer :: lima(nbma)
            character(len=8) :: prefix
            integer :: ndinit
          end subroutine cmlqlq
        end interface
