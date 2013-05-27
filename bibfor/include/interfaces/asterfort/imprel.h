        interface
          subroutine imprel(titre,nbterm,coef,lisddl,lisno,beta)
            integer :: nbterm
            character(*) :: titre
            real(kind=8) :: coef(nbterm)
            character(len=8) :: lisddl(nbterm)
            character(len=8) :: lisno(nbterm)
            real(kind=8) :: beta
          end subroutine imprel
        end interface
