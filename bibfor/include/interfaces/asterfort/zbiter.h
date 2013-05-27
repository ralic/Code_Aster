        interface
          subroutine zbiter(rho,f,rhoopt,fopt,mem,rhonew,echec)
            real(kind=8) :: rho
            real(kind=8) :: f
            real(kind=8) :: rhoopt
            real(kind=8) :: fopt
            real(kind=8) :: mem(2,*)
            real(kind=8) :: rhonew
            logical :: echec
          end subroutine zbiter
        end interface
