        interface
          subroutine fmodam(neq,vite,valmod,basmod,force)
            integer :: neq
            real(kind=8) :: vite(neq)
            character(len=24) :: valmod
            character(len=24) :: basmod
            real(kind=8) :: force(neq)
          end subroutine fmodam
        end interface
