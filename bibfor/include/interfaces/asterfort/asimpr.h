        interface
          subroutine asimpr(nbsup,tcosup,nomsup)
            integer :: nbsup
            integer :: tcosup(nbsup,*)
            character(len=8) :: nomsup(nbsup,*)
          end subroutine asimpr
        end interface
