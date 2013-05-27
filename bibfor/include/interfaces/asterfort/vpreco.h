        interface
          subroutine vpreco(nbvect,neq,vecred,vect)
            integer :: neq
            integer :: nbvect
            real(kind=8) :: vecred(nbvect,nbvect)
            real(kind=8) :: vect(neq,nbvect)
          end subroutine vpreco
        end interface
