        interface
          subroutine ordrel(numnoe,nomnoe,ddl,coef,coefc,nbocno,nbterm&
     &,nomcmp,nddla)
            integer :: nddla
            integer :: nbterm
            integer :: numnoe(nbterm)
            character(len=8) :: nomnoe(nbterm)
            character(len=8) :: ddl(nbterm)
            real(kind=8) :: coef(nbterm)
            complex(kind=8) :: coefc(nbterm)
            integer :: nbocno(nbterm)
            character(len=8) :: nomcmp(nddla)
          end subroutine ordrel
        end interface
