        interface
          subroutine tusief(option,nomte,nbrddl,b,vin,mat,pass,vtemp)
            integer :: nbrddl
            character(len=16) :: option
            character(len=16) :: nomte
            real(kind=8) :: b(4,nbrddl)
            real(kind=8) :: vin(nbrddl)
            real(kind=8) :: mat(4,nbrddl)
            real(kind=8) :: pass(nbrddl,nbrddl)
            real(kind=8) :: vtemp(nbrddl)
          end subroutine tusief
        end interface
