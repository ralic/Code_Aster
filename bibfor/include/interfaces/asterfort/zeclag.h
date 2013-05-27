        interface
          subroutine zeclag(vect,nbddl,ideeq)
            integer :: nbddl
            complex(kind=8) :: vect(nbddl)
            integer :: ideeq(2,nbddl)
          end subroutine zeclag
        end interface
