        interface
          subroutine sstriv(rdiak,rdiam,lprod,ipos,neq)
            integer :: neq
            real(kind=8) :: rdiak(neq)
            real(kind=8) :: rdiam(neq)
            integer :: lprod(neq)
            integer :: ipos(neq)
          end subroutine sstriv
        end interface
