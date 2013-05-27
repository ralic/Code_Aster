        interface
          subroutine tutemp(option,nomte,nbrddl,f,b,vout,pass,vtemp)
            integer :: nbrddl
            character(len=16) :: option
            character(len=16) :: nomte
            real(kind=8) :: f(nbrddl)
            real(kind=8) :: b(4,nbrddl)
            real(kind=8) :: vout(nbrddl)
            real(kind=8) :: pass(nbrddl,nbrddl)
            real(kind=8) :: vtemp(nbrddl)
          end subroutine tutemp
        end interface
