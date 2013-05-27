        interface
          subroutine utacli(x,liste,indmax,tole,indice)
            integer :: indmax
            real(kind=8) :: x
            real(kind=8) :: liste(0:indmax-1)
            real(kind=8) :: tole
            integer :: indice
          end subroutine utacli
        end interface
