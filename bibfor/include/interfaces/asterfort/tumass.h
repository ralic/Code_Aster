        interface
          subroutine tumass(nomte,nbrddl,mass)
            integer :: nbrddl
            character(len=16) :: nomte
            real(kind=8) :: mass(nbrddl,nbrddl)
          end subroutine tumass
        end interface
