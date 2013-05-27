        interface
          subroutine cmcrea(main,maout,nbocc,motfac,numocc)
            integer :: nbocc
            character(len=8) :: main
            character(len=8) :: maout
            character(len=16) :: motfac(nbocc)
            integer :: numocc(nbocc)
          end subroutine cmcrea
        end interface
