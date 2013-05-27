        interface
          subroutine afrela(coefr,coefc,ddl,noeud,ndim,direct,nbterm,&
     &betar,betac,betaf,typcoe,typval,typlag,epsi,lisrez)
            integer :: nbterm
            real(kind=8) :: coefr(nbterm)
            complex(kind=8) :: coefc(nbterm)
            character(len=8) :: ddl(nbterm)
            character(len=8) :: noeud(nbterm)
            integer :: ndim(nbterm)
            real(kind=8) :: direct(3,nbterm)
            real(kind=8) :: betar
            complex(kind=8) :: betac
            character(*) :: betaf
            character(len=4) :: typcoe
            character(len=4) :: typval
            character(len=2) :: typlag
            real(kind=8) :: epsi
            character(*) :: lisrez
          end subroutine afrela
        end interface
