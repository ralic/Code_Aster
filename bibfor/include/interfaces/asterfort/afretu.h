        interface
          subroutine afretu(iprno,lonlis,klisno,noepou,noma,vale1,&
     &nbcoef,idec,coef,nomddl,typlag,lisrel)
            integer :: nbcoef
            integer :: lonlis
            integer :: iprno(*)
            character(len=8) :: klisno(lonlis)
            character(len=8) :: noepou
            character(len=8) :: noma
            character(len=24) :: vale1
            integer :: idec
            real(kind=8) :: coef(nbcoef)
            character(len=8) :: nomddl(nbcoef)
            character(len=2) :: typlag
            character(len=19) :: lisrel
          end subroutine afretu
        end interface
