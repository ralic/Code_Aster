        interface
          subroutine utflm2(mailla,tabmai,nbma,dim,typmai,nbtrou,&
     &tatrou)
            integer :: nbma
            character(len=8) :: mailla
            integer :: tabmai(nbma)
            integer :: dim
            character(*) :: typmai
            integer :: nbtrou
            integer :: tatrou(nbma)
          end subroutine utflm2
        end interface
