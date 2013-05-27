        interface
          subroutine turigi(nomte,nbrddl,k)
            integer :: nbrddl
            character(len=16) :: nomte
            real(kind=8) :: k(nbrddl,nbrddl)
          end subroutine turigi
        end interface
