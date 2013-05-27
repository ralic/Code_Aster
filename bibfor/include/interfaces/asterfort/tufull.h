        interface
          subroutine tufull(option,nomte,nbrddl,deplm,deplp,b,ktild,&
     &effint,pass,vtemp,codret)
            integer :: nbrddl
            character(len=16) :: option
            character(len=16) :: nomte
            real(kind=8) :: deplm(nbrddl)
            real(kind=8) :: deplp(nbrddl)
            real(kind=8) :: b(4,nbrddl)
            real(kind=8) :: ktild(nbrddl,nbrddl)
            real(kind=8) :: effint(nbrddl)
            real(kind=8) :: pass(nbrddl,nbrddl)
            real(kind=8) :: vtemp(nbrddl)
            integer :: codret
          end subroutine tufull
        end interface
