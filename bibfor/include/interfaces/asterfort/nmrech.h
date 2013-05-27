        interface
          subroutine nmrech(fm,f,fopt,fcvg,rhomin,rhomax,rhoexm,rhoexp&
     &,rhom,rho,rhoopt,ldcopt,ldccvg,opt,act,stite)
            real(kind=8) :: fm
            real(kind=8) :: f
            real(kind=8) :: fopt
            real(kind=8) :: fcvg
            real(kind=8) :: rhomin
            real(kind=8) :: rhomax
            real(kind=8) :: rhoexm
            real(kind=8) :: rhoexp
            real(kind=8) :: rhom
            real(kind=8) :: rho
            real(kind=8) :: rhoopt
            integer :: ldcopt
            integer :: ldccvg
            integer :: opt
            integer :: act
            logical :: stite
          end subroutine nmrech
        end interface
