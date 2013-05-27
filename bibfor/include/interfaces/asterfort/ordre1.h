        interface
          subroutine ordre1(numcle,nomnoe,ddl,coef,coefc,nbterm)
            integer :: nbterm
            integer :: numcle(nbterm)
            character(len=8) :: nomnoe(nbterm)
            character(len=8) :: ddl(nbterm)
            real(kind=8) :: coef(nbterm)
            complex(kind=8) :: coefc(nbterm)
          end subroutine ordre1
        end interface
