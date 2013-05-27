        interface
          subroutine utmavo(mail,kdim,lima,nlima,base,nomz,nbmavo,&
     &mailvo)
            character(len=8) :: mail
            character(len=2) :: kdim
            integer :: lima(*)
            integer :: nlima
            character(len=1) :: base
            character(*) :: nomz
            integer :: nbmavo
            integer :: mailvo(*)
          end subroutine utmavo
        end interface
