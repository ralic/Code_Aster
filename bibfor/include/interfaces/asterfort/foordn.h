        interface
          subroutine foordn(vecpar,vecnom,ne,ns,ier)
            integer :: ne
            real(kind=8) :: vecpar(ne)
            character(*) :: vecnom(ne)
            integer :: ns
            integer :: ier
          end subroutine foordn
        end interface
