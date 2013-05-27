        interface
          subroutine kcoude(nbrddl,poids,b,c,k)
            integer :: nbrddl
            real(kind=8) :: poids
            real(kind=8) :: b(4,nbrddl)
            real(kind=8) :: c(4,4)
            real(kind=8) :: k(nbrddl,nbrddl)
          end subroutine kcoude
        end interface
