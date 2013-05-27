        interface
          subroutine calatm(neq,nbddl,mu,coef,ddl,atmu)
            integer :: nbddl
            integer :: neq
            real(kind=8) :: mu
            real(kind=8) :: coef(nbddl)
            integer :: ddl(nbddl)
            real(kind=8) :: atmu(neq)
          end subroutine calatm
        end interface
