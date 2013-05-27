        interface
          subroutine tuforc(option,nomte,nbrddl,b,f,vin,vout,mat,pass,&
     &vtemp)
            integer :: nbrddl
            character(*) :: option
            character(len=16) :: nomte
            real(kind=8) :: b(4,nbrddl)
            real(kind=8) :: f(nbrddl)
            real(kind=8) :: vin(nbrddl)
            real(kind=8) :: vout(nbrddl)
            real(kind=8) :: mat(nbrddl,4)
            real(kind=8) :: pass(nbrddl,nbrddl)
            real(kind=8) :: vtemp(nbrddl)
          end subroutine tuforc
        end interface
