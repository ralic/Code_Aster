        interface
          subroutine foec2n(iuni,vecpro,valpar,chval,nbfonc,impr)
            integer :: nbfonc
            integer :: iuni
            character(*) :: vecpro(*)
            real(kind=8) :: valpar(nbfonc)
            character(*) :: chval
            integer :: impr
          end subroutine foec2n
        end interface
