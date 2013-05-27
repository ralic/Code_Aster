        interface
          subroutine nmrebo(f,mem,sens,rho,rhoopt,ldcopt,ldccvg,fopt,&
     &fcvg,opt,act,rhomin,rhomax,rhoexm,rhoexp,stite,echec)
            real(kind=8) :: f
            real(kind=8) :: mem(2,*)
            real(kind=8) :: sens
            real(kind=8) :: rho
            real(kind=8) :: rhoopt
            integer :: ldcopt
            integer :: ldccvg
            real(kind=8) :: fopt
            real(kind=8) :: fcvg
            integer :: opt
            integer :: act
            real(kind=8) :: rhomin
            real(kind=8) :: rhomax
            real(kind=8) :: rhoexm
            real(kind=8) :: rhoexp
            logical :: stite
            logical :: echec
          end subroutine nmrebo
        end interface
