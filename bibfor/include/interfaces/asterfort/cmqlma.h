        interface
          subroutine cmqlma(main,maout,nbma,mailq)
            integer :: nbma
            character(len=8) :: main
            character(len=8) :: maout
            integer :: mailq(nbma)
          end subroutine cmqlma
        end interface
