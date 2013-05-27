        interface
          subroutine foec2c(iuni,v,nbcoup,n1,n2,nompar,nomres)
            integer :: nbcoup
            integer :: iuni
            real(kind=8) :: v(3*nbcoup)
            integer :: n1
            integer :: n2
            character(*) :: nompar
            character(*) :: nomres
          end subroutine foec2c
        end interface
