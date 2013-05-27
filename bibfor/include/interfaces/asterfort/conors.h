        interface
          subroutine conors(i1,i2,i3,macoc,nbcoc,macor,nbcor,loreor,&
     &mailla)
            integer :: nbcor
            integer :: nbcoc
            integer :: i1
            integer :: i2
            integer :: i3
            character(len=8) :: macoc(2+nbcoc)
            character(len=8) :: macor(2+nbcor)
            logical :: loreor
            character(len=8) :: mailla
          end subroutine conors
        end interface
