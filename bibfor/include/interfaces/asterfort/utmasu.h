        interface
          subroutine utmasu(mail,kdim,nlima,lima,nomob1,coor,nbmavo,&
     &mailvo,coince)
            character(len=8) :: mail
            character(len=2) :: kdim
            integer :: nlima
            integer :: lima(*)
            character(*) :: nomob1
            real(kind=8) :: coor(*)
            integer :: nbmavo
            integer :: mailvo(*)
            logical :: coince
          end subroutine utmasu
        end interface
