        interface
          subroutine cmcovo(main,maout,nbma,lima,prefno,prefma,inima,&
     &epais,plan,trans)
            integer :: nbma
            character(len=8) :: main
            character(len=8) :: maout
            integer :: lima(nbma)
            character(len=8) :: prefno
            character(len=8) :: prefma
            integer :: inima
            real(kind=8) :: epais
            character(len=8) :: plan
            character(len=8) :: trans
          end subroutine cmcovo
        end interface
