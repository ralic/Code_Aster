        interface
          subroutine mmmvee(phasez,ndim,nne,norm,tau1,tau2,mprojt,wpg,&
     &ffe,jacobi,jeu,coefac,coefaf,lambda,coefff,dlagrc,dlagrf,dvite,&
     &rese,nrese,vectee)
            character(*) :: phasez
            integer :: ndim
            integer :: nne
            real(kind=8) :: norm(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: wpg
            real(kind=8) :: ffe(9)
            real(kind=8) :: jacobi
            real(kind=8) :: jeu
            real(kind=8) :: coefac
            real(kind=8) :: coefaf
            real(kind=8) :: lambda
            real(kind=8) :: coefff
            real(kind=8) :: dlagrc
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: dvite(3)
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: vectee(27)
          end subroutine mmmvee
        end interface
