        interface
          subroutine nmunil(mailla,depplu,ddepla,solveu,matass,deficu,&
     &resocu,cncine,iterat,inst,ctccvg)
            character(len=8) :: mailla
            character(len=19) :: depplu
            character(len=19) :: ddepla
            character(len=19) :: solveu
            character(len=19) :: matass
            character(len=24) :: deficu
            character(len=24) :: resocu
            character(len=19) :: cncine
            integer :: iterat
            real(kind=8) :: inst
            integer :: ctccvg
          end subroutine nmunil
        end interface
