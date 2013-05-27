        interface
          subroutine mefrac(mailla,nbgrmx,nomrac,nbgrma,nomcyl)
            character(len=8) :: mailla
            integer :: nbgrmx
            character(len=24) :: nomrac
            integer :: nbgrma
            character(len=24) :: nomcyl(*)
          end subroutine mefrac
        end interface
