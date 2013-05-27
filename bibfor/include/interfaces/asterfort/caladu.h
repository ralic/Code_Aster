        interface
          subroutine caladu(neq,nbddl,coef,ddl,depl,val)
            integer :: nbddl
            integer :: neq
            real(kind=8) :: coef(nbddl)
            integer :: ddl(nbddl)
            real(kind=8) :: depl(neq)
            real(kind=8) :: val
          end subroutine caladu
        end interface
