        interface
          subroutine conjac(i0,i1,i2,i3,macoc,nbcoc,mailla)
            integer :: nbcoc
            integer :: i0
            integer :: i1
            integer :: i2
            integer :: i3
            character(len=8) :: macoc(2+nbcoc)
            character(len=8) :: mailla
          end subroutine conjac
        end interface
